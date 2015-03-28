/*
 * Created on Mar 23, 2007
 * Copyright (C) 2001-5, Anthony Harrison anh23@pitt.edu (jactr.org) This library is free
 * software; you can redistribute it and/or modify it under the terms of the GNU
 * Lesser General Public License as published by the Free Software Foundation;
 * either version 2.1 of the License, or (at your option) any later version.
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details. You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 */
package de.monochromata.jactr.instruments.activation;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.concurrent.ExecutorService;

import org.apache.commons.logging.Log; //standard logging support
import org.apache.commons.logging.LogFactory;
import org.jactr.core.buffer.IActivationBuffer;
import org.jactr.core.concurrent.ExecutorServices;
import org.jactr.core.model.IModel;
import org.jactr.core.model.event.IModelListener;
import org.jactr.core.runtime.ACTRRuntime;
import org.jactr.core.utils.parameter.IParameterized;
import org.jactr.instrument.IInstrument;

/**
 * Exports a chunk graph and activation of the chunks over time. The output
 * is to be processed via the dmGraph.r script that is able to create PDFs.
 * 
 * TODO: Write a GUI element to process the output directly.
 *
 */
public class ActivationGraph implements IInstrument, IParameterized {
	
	public static final String PATH_TO_CONFIG_XML = "pathToConfigXML";
	
	private static final transient Log LOGGER = LogFactory
			.getLog(ActivationGraph.class);

	private PrintWriter logWriter;
	private ChunkExporter chunkExporter;
	private List<String> possibleParameters;
	private String pathToConfigXML;
	private Configuration configuration;
	
	public ActivationGraph() {
	}
	
	// Instrument implementation (following dmGraph.r)
	
	/**
	 * initialize is called after install but before the model run starts
	 * 
	 * @see org.jactr.instrument.IInstrument#initialize()
	 */
	public void initialize() {

	}

	public void install(IModel model) {
		
		// Add listener to all configured buffers
		try {
			Configuration config = getConfiguration();
			logWriter = new PrintWriter(new BufferedWriter(
					new FileWriter(new File(ACTRRuntime.getRuntime().getWorkingDirectory(),
							config.getOutputFileName()))));
			int maxGraphDepth = config.getMaxGraphDepth();
			int sampleDurationMs = config.getSampleDurationMs();
			chunkExporter = new ChunkExporter(logWriter, config.getBuffersToGraph(),
					config.getBuffersToTrackEventsFor(),
					config.getLogSpecs(), maxGraphDepth, sampleDurationMs);
			
			for (IActivationBuffer buffer : model.getActivationBuffers()) {
				if(config.getBuffersToGraph().contains(buffer.getName())) {
					buffer.addListener(chunkExporter, ExecutorServices.INLINE_EXECUTOR);
				}
			}
		} catch (IOException ioe) {
			LOGGER.error("Failed to install ActivationGraph: "+ioe.getMessage(), ioe);
		}
		
		// Add model listener
		model.addListener((IModelListener)chunkExporter, ExecutorServices.INLINE_EXECUTOR);
	}

	/**
	 * unlike modules, instruments can be uninstalled, this is a good time to
	 * release or flush resources
	 * 
	 * @see org.jactr.instrument.IInstrument#uninstall(org.jactr.core.model.IModel)
	 */
	public void uninstall(IModel model) {
		// Remove chunk exporter from notifiers
		for(IActivationBuffer buffer: model.getActivationBuffers()) {
			buffer.removeListener(chunkExporter);
		}
		model.removeListener((IModelListener)chunkExporter);
		chunkExporter = null;
		
		// Close log writer
		logWriter.flush();
		logWriter.close();
		logWriter = null;
	}
	
	// Parameter handling
	
	private Configuration getConfiguration() {
		if(configuration == null) {
			String configFileName = getParameter(PATH_TO_CONFIG_XML);
			try {
				configuration = Configuration.loadFromFile(configFileName);
				if(configuration == null) {
					configuration = Configuration.getDefault();
					Configuration.saveToFile(configFileName, configuration);
				}
			} catch (IOException ioe) {
				LOGGER.error("Failed to load configuration from "+configFileName
						+", using default configuration: "+ioe.getMessage(), ioe);
				configuration = new Configuration();
			}
		}
		return configuration;
	}

	@Override
	public void setParameter(String key, String value) {
		if(key.equals(PATH_TO_CONFIG_XML)) {
			pathToConfigXML = value;
		} else {
			throw new IllegalArgumentException("Unknown parameter: "+key);
		}
	}

	@Override
	public String getParameter(String key) {
		if(key.equals(PATH_TO_CONFIG_XML)) {
			return pathToConfigXML;
		} else {
			throw new IllegalArgumentException("Unknown parameter "+key);
		}
	}
	
	@Override
	public Collection<String> getPossibleParameters() {
		if(possibleParameters == null) {
			possibleParameters = Arrays.asList(new String[] {
				PATH_TO_CONFIG_XML
			});
		}
		return possibleParameters;
	}

	@Override
	public Collection<String> getSetableParameters() {
		return getPossibleParameters();
	}

}
