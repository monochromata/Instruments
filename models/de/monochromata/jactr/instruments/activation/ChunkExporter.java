package de.monochromata.jactr.instruments.activation;

import java.awt.image.BufferedImageFilter;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Locale;

import org.jactr.core.buffer.IActivationBuffer;
import org.jactr.core.buffer.event.ActivationBufferEvent;
import org.jactr.core.buffer.event.ActivationBufferListenerAdaptor;
import org.jactr.core.chunk.IChunk;
import org.jactr.core.chunk.ISubsymbolicChunk;
import org.jactr.core.chunk.ISymbolicChunk;
import org.jactr.core.model.IModel;
import org.jactr.core.model.event.IModelListener;
import org.jactr.core.model.event.ModelEvent;
import org.jactr.core.slot.ISlot;

import static java.util.Locale.*;

/**
 * 
 * @author monochromata
 *
 */
public class ChunkExporter extends ActivationBufferListenerAdaptor implements IModelListener {

	private final PrintWriter logWriter;
	private final List<String> buffersToGraph,
							   buffersToTrackForEvents;
	private final List<LogSpec> logSpecs;
	private double lastExport = Double.MIN_VALUE;
	private final int maxGraphDepth;
	private final double sampleDurationS;
	private final List<IChunk> chunksToExport = new ArrayList<>();
	
	/**
	 * Creates a new chunk exporter and sets the default locale to {@link Locale#ENGLISH}
	 * to ensure correct decimal formats. 
	 * 
	 * @param logWriter
	 * @param buffersToGraph
	 * @param buffersToTrackForEvents
	 * @param logSpecs
	 * @param maxGraphDepth
	 * @param sampleDurationMs
	 */
	public ChunkExporter(PrintWriter logWriter, List<String> buffersToGraph,
			List<String> buffersToTrackForEvents,
			List<LogSpec> logSpecs, int maxGraphDepth, int sampleDurationMs) {
		this(logWriter, buffersToGraph, buffersToTrackForEvents, logSpecs,
				maxGraphDepth, sampleDurationMs, Locale.ENGLISH);
	}
	
	public ChunkExporter(PrintWriter logWriter, List<String> buffersToGraph,
			List<String> buffersToTrackForEvents,
			List<LogSpec> logSpecs, int maxGraphDepth, int sampleDurationMs,
			Locale locale) {	
		this.logWriter = logWriter;
		this.buffersToGraph = buffersToGraph;
		this.buffersToTrackForEvents = buffersToTrackForEvents;
		this.logSpecs = logSpecs;
		this.maxGraphDepth = maxGraphDepth;
		this.sampleDurationS = ((double)sampleDurationMs)/1000.0;
		if(locale != null) {
			Locale.setDefault(locale);
		}
	}

	// Maintain list of chunks to export (IActivationBufferListener)
	// and trigger logging of buffer events.
	
	@Override
	public void sourceChunkAdded(ActivationBufferEvent abe) {
		
		// Maybe add chunk to export
		IModel model = abe.getSource().getModel();
		for(IChunk chunk: abe.getSourceChunks()) {
			if(matchesALogSpec(chunk, model)) {
				findVertices(chunk, model, 0);
			}
		}
		
		// Maybe log buffer event, but not before the first chunk to
		// export is available.
		if(!chunksToExport.isEmpty()) {
			String bufferName = abe.getSource().getName();
			for(String bufferToTrack: buffersToTrackForEvents) {
				if(bufferToTrack.equals(bufferName)) {
					logBufferEvent(abe);
				}
			}
		}
	}
	
	@SuppressWarnings("unchecked")
	private void findVertices(IChunk chunk, IModel model, int depth) {
		if(chunk != null && depth <= maxGraphDepth) {
			// maybe add the chunk
			if(!chunksToExport.contains(chunk)) {
				chunksToExport.add(chunk);
			}
			// add slot values
			for(ISlot slot: chunk.getSymbolicChunk().getSlots()) {
				Object value = slot.getValue();
				if(value instanceof Collection) {
					for(Object element: (Collection<Object>)value) {
						if(element instanceof IChunk) {
							findVertices((IChunk)element, model, depth+1);
						}
					}
				} else if(value instanceof IChunk) {
					findVertices((IChunk)value, model, depth+1);
				}
			}
		}
	}
	
	private boolean matchesALogSpec(IChunk chunk, IModel model) {
		for(LogSpec logSpec: logSpecs) {
			if(logSpec.shouldLog(chunk, model)) {
				return true;
			}
		}
		return false;
	}
	
	private void logBufferEvent(ActivationBufferEvent abe) {
		int nextStimulusIndex = 0; // kept for compatibility with dmGraphR.lisp
		String bufferAction = "SET-BUFFER-CHUNK"; // dito
		logWriter.println(String.format("event %1$d %2$5.3f %3$s %4$s %5$s",
				nextStimulusIndex, abe.getSimulationTime(),
				abe.getSource().getName(), bufferAction,
				getChunkNames(abe.getSourceChunks())));
	}

	private String getChunkNames(Collection<IChunk> chunks) {
		String chunkNames= "";
		for(IChunk chunk: chunks) {
			chunkNames += (chunkNames.equals("")?"":",") + chunk.getSymbolicChunk().getName();
		}
		return chunkNames;
	}
	
	// Export chunks (IModelListener)

	@Override
	public void modelConnected(ModelEvent me) {}
	@Override
	public void modelDisconnected(ModelEvent me) {}
	@Override
	public void moduleInstalled(ModelEvent me) {}
	@Override
	public void extensionInstalled(ModelEvent me) {}
	@Override
	public void instrumentInstalled(ModelEvent me) {}
	@Override
	public void bufferInstalled(ModelEvent me) {}
	@Override
	public void modelInitialized(ModelEvent me) {}
	@Override
	public void modelStarted(ModelEvent me) {}
	@Override
	public void modelSuspended(ModelEvent me) {}
	@Override
	public void modelResumed(ModelEvent me) {}
	@Override
	public void modelStopped(ModelEvent me) {}
	@Override
	public void exceptionThrown(ModelEvent me) {}
	
	@Override
	public void cycleStarted(ModelEvent me) {
		if(lastExport+sampleDurationS <= me.getSimulationTime()) {
			lastExport = me.getSimulationTime();
			exportData(me.getSource(), lastExport);
		}
	}
	
	@Override
	public void cycleStopped(ModelEvent me) {}
	
	private void exportData(IModel model, double exportTime) {
		
		int nextStimulusIndex = 0; // kept for compatibility with dmGraphR.lisp
		
		List<String> vertexInfo = new ArrayList<>();
		List<String> edgeInfo = new ArrayList<>();
		for(IChunk chunk: chunksToExport) {
			exportData(chunk, model, vertexInfo, edgeInfo, exportTime);
		}
		
		// Export buffer data, but not before there are chunks to export
		if(!chunksToExport.isEmpty()) {
			for(String bufferName: buffersToGraph) {
				IActivationBuffer buffer = model.getActivationBuffer(bufferName);
				String sourceChunkNames = getChunkNames(buffer.getSourceChunks());
				logWriter.println(String.format("buffer %1$d %2$5.3f %3$s %4$4.2f %5$s %6$s",
						nextStimulusIndex, exportTime, buffer.getName(),
						buffer.getActivation(),
						sourceChunkNames, sourceChunkNames)); // TODO: second names might be beautified
			}
		}
		
		// Export vertex and edge data
		vertexInfo.forEach(logWriter::println);
		edgeInfo.forEach(logWriter::println);
	}
	
	@SuppressWarnings("unchecked")
	private void exportData(IChunk chunk, IModel model,
			List<String> vertexInfo, List<String> edgeInfo,
			double exportTime) {
		
		int nextStimulusIndex = 0; // kept for compatibility with dmGraphR.lisp
		
		ISymbolicChunk sc = chunk.getSymbolicChunk();
		ISubsymbolicChunk ssc = chunk.getSubsymbolicChunk();
		
		// Create vertex attributes
		double blAct = ssc.getReferences().getNumberOfReferences() > 0?
				ssc.getBaseLevelActivation():0;
		double sAct = ssc.getSpreadingActivation();
		double totalAct = ssc.getActivation();
		vertexInfo.add(String.format("chunk %1$d %2$5.3f %3$s %4$s %5$4.2f %6$4.2f %7$4.2f",
						nextStimulusIndex, exportTime,
						sc.getName(), sc.getName(), // TODO: second name might be beautified
						totalAct, blAct, sAct));
		
		// Create edge attributes (i.e. Wkj and Sji values)
		for(ISlot slot: sc.getSlots()) {
			String slotName = slot.getName();
			Object value = slot.getValue();
			int i=0;
			if(value instanceof Collection) {
				for(Object element: (Collection<Object>)value) {
					String elementSlotName = slotName+(i++);
					collectSlotData(model, nextStimulusIndex, exportTime, chunk,
							elementSlotName, element, edgeInfo);
				}
			} else {
				String elementSlotName = slotName+(i++);
				collectSlotData(model, nextStimulusIndex, exportTime, chunk,
						elementSlotName, value, edgeInfo);				
			}
		}
	}
	
	private void collectSlotData(IModel model, int nextStimulusIndex,
			double exportTime, IChunk chunk, String slotName, Object slotValue,
			List<String> edges) {
		if(slotValue != null
				&& slotValue instanceof IChunk
				/*&& matchesALogSpec(chunk, model)*/) {
				int indexOfValue = chunksToExport.indexOf((IChunk)slotValue);
				double Sji = 0.0; // TODO: how to get this value?
				if(indexOfValue != -1) {
					// only links to known chunks will be exported
					int indexOfChunk = chunksToExport.indexOf(chunk);
					edges.add(String.format("slot %1$d %2$5.3f %3$d %4$d %5$s %6$4.2f",
									nextStimulusIndex, exportTime,
									1 + indexOfChunk, 1 + indexOfValue, // convert 0- to 1-based indices
									slotName, Sji));
				}
			}		
	}
	
	
}
