package de.monochromata.jactr.instruments;

import static de.monochromata.jactr.twm.ITWM.getObjectVsMethod;

import java.io.PrintWriter;
import java.util.Arrays;
import java.util.Collection;

import org.jactr.core.chunk.IChunk;
import org.jactr.core.chunk.ISymbolicChunk;
import org.jactr.core.model.IModel;
import org.jactr.core.slot.ISlot;

import de.monochromata.jactr.tls.CollectionSlot;
import de.monochromata.jactr.tls.ICollectionSlot;
import de.monochromata.jactr.twm.ITWM;
import de.monochromata.jactr.twm.ITWMListener;
import de.monochromata.jactr.twm.TWMAdapter;

public class ErrorLog extends AbstractInstrument {

	public static final String ERROR_LOG_FILE_NAME = "errorLogFileName";
	
	private PrintWriter logWriter;
	private LoggingListener loggingListener;

	@Override
	protected Collection<String> createPossibleParameters() {
		return Arrays.asList(new String[] {
			ERROR_LOG_FILE_NAME
		});
	}
	
	@Override
	public void install(IModel model) {
		ITWM twm = (ITWM)model.getModule(ITWM.class);
		String logFileName = getParameter(ERROR_LOG_FILE_NAME);
		logWriter = getLogWriter(logFileName,
				() -> { return logWriter; },
				writer -> { logWriter = writer; });
		loggingListener = new LoggingListener(logWriter);
		twm.addListener(loggingListener);
	}

	@Override
	public void uninstall(IModel model) {
		ITWM twm = (ITWM)model.getModule(ITWM.class);
		twm.removeListener(loggingListener);
		logWriter.flush();
		logWriter.close();
		logWriter = null;
		loggingListener = null;
	}

	@Override
	public void initialize() {
	}

	private static class LoggingListener extends TWMAdapter {

		private final PrintWriter logWriter;
		
		public LoggingListener(PrintWriter logWriter) {
			this.logWriter = logWriter;
		}

		@Override
		public void specify(IChunk featureReceiver, IChunk featureProvider,
				double startTime, double endTime, IChunk requestChunk) {

			if(featureReceiver.getSymbolicChunk().getName().equals("error")) {
				logWriter.println(String.format("%1$5.2f Could not find feature receiver, cannot specify"
						+" (request for %2$s to end at %3$5.2f, graphemic=%4$s).",
						startTime, requestChunk, endTime, getGraphemic(requestChunk)));
			} else if(featureProvider.getSymbolicChunk().getName().equals("error")) {
				logWriter.println(String.format("%1$5.2f Could not find feature provider, cannot specify"
						+" (request for %2$s to end at %3$5.2f, graphemic=%4$s).",
						startTime, requestChunk, endTime, getGraphemic(requestChunk)));
			} else {
				// Make sure that methods and objects do not get mixed
				String receiverOvM = getObjectVsMethod(featureReceiver);
				String providerOvM = getObjectVsMethod(featureProvider);
				if(!receiverOvM.equals(providerOvM)) {
					logWriter.println(String.format("%1$5.2f Illegal specification to be finished at %2$5.2f:"
							+"Cannot mix  method and object schemata: %3$s vs. %4$s (request for %5$s, graphemic=%6$s)",
							startTime, endTime, featureProvider, featureReceiver, requestChunk, getGraphemic(requestChunk)));
				}
			}
		}

		@Override
		public void instantiate(IChunk typeSchema, double startTime,
				double endTime, IChunk requestChunk) {
			
			if(typeSchema.getSymbolicChunk().getName().equals("error")) {
				logWriter.println(String.format("%1$5.2f Cannot instantiate: no type schema found"
						+" (request for %2$s,graphemic=%3$s to complete at %4$5.2f)",
						startTime, requestChunk, getGraphemic(requestChunk), endTime ));
			} else {					
				ISlot typeVsTokenSlot = typeSchema.getSymbolicChunk().getSlot("typeVsToken");
				String value;
				if(typeVsTokenSlot instanceof ICollectionSlot) {
					value = (String)((ICollectionSlot)typeVsTokenSlot).getValues().iterator().next();
				} else {
					value = (String)typeVsTokenSlot.getValue();
				}
				if(!value.equals("type")) {
					logWriter.println(String.format("%1$5.2f Instantiating token schema %2$s"
							+" - should be a type schema (request for %3$s, graphemic=%4$s"
							+" to complete at %5$5.2f)",
							startTime, typeSchema, requestChunk, getGraphemic(requestChunk), endTime));
				}
			}
		}

		@Override
		public void linkArgumentToReferentOfCallableInvocation(String roleIn,
				IChunk callableInvocation, IChunk callableInvocationReferent,
				String roleId, IChunk oldArgument, IChunk newArgument, double startTime,
				double endTime, IChunk requestChunk) {
			if(callableInvocationReferent == null) {
				logWriter.println(String.format("%1$5.2f Method invocation has no referent event though"
						+ "it is instantiated %2$s (request for %3$s, graphemic=%4$s to complete at %5$5.2f)",
						startTime, callableInvocation, requestChunk, getGraphemic(requestChunk), endTime));
			} else if(oldArgument == null){
				ISymbolicChunk sc = callableInvocationReferent.getSymbolicChunk();
				logWriter.println(String.format("%1$5.2f No chunk found for roleId=%2$s"
						+" using roleIn=%3$s in referent=%4$s=%5$s to replace by %6$s"
						+" (request for %7$s, graphemic=%8$s to complete at %9$5.2f)",
						startTime, roleId, roleIn, sc.getName(), sc.getSlots(), newArgument,
						requestChunk, getGraphemic(requestChunk), endTime));
			}
		}

		@Override
		public void linkReferentOfCallableInvocationToArgumentsAndUpdateRoleIds(
				IChunk referencePotential, IChunk referent, String declaredIn,
				ISlot roleContainer, String roleId, IChunk role,
				String argumentReferencePotentialId,
				IChunk argumentReferencePotential, IChunk argumentReferent,
				double startTime, double endTime, IChunk requestChunk) {
			if(argumentReferencePotential != null
					&& argumentReferent != null
					&& roleContainer == null) { // role will be null, too
				logWriter.println(String.format("%1$5.2f Failed to replace #%2$s: "
						+" by %3$s (referred to by %4$s, graphemic=%5$s) in %6$s(graphemic=%7$s)"
						+".features=%8$s (request for %9$s, graphemic=%10$s to complete at %11$5.2f)",
						startTime, roleId, argumentReferent, argumentReferencePotential,
						getGraphemic(argumentReferencePotential), referent,
						getGraphemic(referent), roleContainer, requestChunk, getGraphemic(requestChunk), endTime));
			}
		}

		@Override
		public void access(String kindOfChunk, IChunk chunk, double startTime,
				double endTime, IChunk requestChunk) {
			if(chunk == null) {
				logWriter.println(String.format("%1$5.2f Cannot add access at %2$5.2f to "
					+"null %3$s chunk after processing request for %4$s (graphemic=%5$s)",
					startTime, endTime, kindOfChunk, requestChunk, getGraphemic(requestChunk)));
			}
		}
		
		public String getGraphemic(IChunk chunk) {
			ISlot graphemicSlot = chunk.getSymbolicChunk().getSlot("graphemic");
			if(graphemicSlot instanceof ICollectionSlot) {
				Collection<Object> values = ((ICollectionSlot)graphemicSlot).getValues();
				if(values.size() != 1) {
					throw new IllegalStateException("Invalid number of graphemic representations: "+values.size()+" in "+chunk);
				} else {
					return (String)values.iterator().next();
				}
			} else {
				return (String)graphemicSlot.getValue();
			}
		}
	}
}
