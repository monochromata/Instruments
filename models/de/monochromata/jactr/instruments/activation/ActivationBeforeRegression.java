package de.monochromata.jactr.instruments.activation;

import java.io.PrintWriter;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ExecutionException;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.jactr.core.chunk.IChunk;
import org.jactr.core.model.IModel;

import de.monochromata.jactr.dm.INonMergingDeclarativeModule;
import de.monochromata.jactr.instruments.AbstractInstrument;
import de.monochromata.jactr.remma.Fixation;
import de.monochromata.jactr.remma.Fixation.Word;
import de.monochromata.jactr.remma.IREMMA;
import de.monochromata.jactr.remma.IREMMAListener;
import de.monochromata.jactr.tls.AnaphorInfo;
import de.monochromata.jactr.twm.ITWM;
import de.monochromata.jactr.twm.ITWMListener;
import de.monochromata.jactr.twm.TWMAdapter;

public class ActivationBeforeRegression extends AbstractInstrument {

	private static final transient Log LOGGER = LogFactory.getLog(ActivationBeforeRegression.class);

	public static final String PARAM_ACTIVATION_BEFORE_REGRESSION_LOG_FILE_NAME = "activationBeforeRegressionLogFileName";
	public static final String PARAM_LOG_SEPARATOR = "logSeparator";
	
	private Map<Integer,UnderspecifiedRelation> underspecifiedRelations = new HashMap<>();
	private PrintWriter logWriter;
	private LoggingListener listener;
	
	public ActivationBeforeRegression() {
		initUnderspecifiedRelations();
	}
	
	protected void initUnderspecifiedRelations() {
		underspecifiedRelations.put( 7, new UnderspecifiedRelation(- 8.158, 'L', "CS#RegistrarLocator.getRegistrarARGSSGRA.return.type"));
		underspecifiedRelations.put( 8, new UnderspecifiedRelation(- 9.579, 'L', "CS#OutriggerImpl.spaceProxy.featuresOf.SpaceProxy.type"));
		underspecifiedRelations.put( 9, new UnderspecifiedRelation(-14.947, 'L', "CS#ServiceRegistrar.registerARGSServiceItem_longSGRA.return.type"));
		underspecifiedRelations.put(10, new UnderspecifiedRelation(-12.579, 'L', "CS#ServiceRegistration.getLeaseARGSSGRA.return.type"));
		underspecifiedRelations.put(11, new UnderspecifiedRelation(-22.579, 'L', "CS#OutriggerImpl.lrm.featuresOf.LeaseRenewalManager.type"));
		underspecifiedRelations.put(45, new UnderspecifiedRelation(- 5.263, 'H', "CS#DiscoveryEvent.getRegistrarsARGSSGRA.return.type"));
		underspecifiedRelations.put(46, new UnderspecifiedRelation(- 0.737, 'H', "CS#Service.getServiceIDARGSSGRA.return.type"));
		underspecifiedRelations.put(47, new UnderspecifiedRelation(- 4.158, 'H', "CS#ServiceRegistrar.getLocatorARGSSGRA.return.type"));
		underspecifiedRelations.put(48, new UnderspecifiedRelation(- 6.158, 'L', "CS#ServiceRegistrar.lookupARGSServiceTemplate_intSGRA.return.type"));
		underspecifiedRelations.put(55, new UnderspecifiedRelation(  1.842, 'H', "CS#Service.getServiceIDARGSSGRA.return.type"));
		underspecifiedRelations.put(59, new UnderspecifiedRelation(- 0.842, 'H', "CS#Voucher.location.featuresOf.Location.type"));
		underspecifiedRelations.put(60, new UnderspecifiedRelation(  2.421, 'H', "CS#Service.getServiceIDARGSSGRA.return.type"));
		underspecifiedRelations.put(67, new UnderspecifiedRelation(- 3.684, 'H', "CS#DiscoveryEvent.getRegistrarsARGSSGRA.return.type"));
		underspecifiedRelations.put(68, new UnderspecifiedRelation(  2,     'H', "CS#Service.getServiceIDARGSSGRA.return.type"));
		underspecifiedRelations.put(69, new UnderspecifiedRelation(- 4.579, 'H', "CS#ServiceRegistrar.getLocatorARGSSGRA.return.type"));
		underspecifiedRelations.put(70, new UnderspecifiedRelation(- 6,     'L', "CS#ServiceRegistrar.lookupARGSServiceTemplate_intSGRA.return.type"));
		underspecifiedRelations.put(79, new UnderspecifiedRelation(-19.895, 'L', "CS#JavaSpace.addListenerARGSEntry_RemoteEventListener_longSGRA.return.type"));
		underspecifiedRelations.put(80, new UnderspecifiedRelation(-14.421, 'L', "CS#Service.getServiceIDARGSSGRA.return.type"));
		underspecifiedRelations.put(81, new UnderspecifiedRelation(-10.842, 'L', "CS#EventRegistration.getLeaseARGSSGRA.return.type"));
	}
	
	@Override
	protected Collection<String> createPossibleParameters() {
		return Arrays.asList(new String[] {
				PARAM_ACTIVATION_BEFORE_REGRESSION_LOG_FILE_NAME,
				PARAM_LOG_SEPARATOR
		});
	}
	
	// IInstrument implementation

	@Override
	public void install(IModel model) {
		INonMergingDeclarativeModule dm = (INonMergingDeclarativeModule)model.getDeclarativeModule();
		ITWM twm = (ITWM)model.getModule(ITWM.class);
		String logFileName = getParameter(PARAM_ACTIVATION_BEFORE_REGRESSION_LOG_FILE_NAME);
		logWriter = getLogWriter(logFileName,
				() -> { return logWriter; },
				writer -> { logWriter = writer; });
		listener = new LoggingListener(dm, logWriter);
		twm.addListener(listener);
	}

	@Override
	public void uninstall(IModel model) {
		ITWM twm = (ITWM)model.getModule(ITWM.class);
		twm.removeListener(listener);
		listener = null;
		logWriter.flush();
		logWriter.close();
		logWriter = null;
	}

	@Override
	public void initialize() { }

	private class LoggingListener extends TWMAdapter {

		private final INonMergingDeclarativeModule dm;
		private final PrintWriter logWriter;
		private AnaphorInfo currentAnaphorInfo;
		private Set<Integer> seenAnaphors = new HashSet<>();
		private Set<Integer> specifiedOrInstantiatedAnaphors = new HashSet<>();
		
		public LoggingListener(INonMergingDeclarativeModule dm, PrintWriter logWriter) {
			this.dm = dm;
			this.logWriter = logWriter;
		}

		@Override
		public void requestWordActivation(double requestTime, IChunk wordChunk) {
			maybeLogActivationAtRegressionStart(requestTime, wordChunk);
		}

		@Override
		public void requestReferentialisation(double requestTime, IChunk referencePotentialChunk) {
			maybeLogActivationAtRegressionStart(requestTime, referencePotentialChunk);
		}

		@Override
		public void specify(IChunk featureReceiver, IChunk featureProvider,
				double startTime, double endTime, IChunk requestChunk) {
			maybeLogComputedActivation("Specifying", startTime, featureReceiver);
		}

		@Override
		public void instantiate(IChunk typeSchema, double startTime,
				double endTime, IChunk requestChunk) {
			maybeLogComputedActivation("Instantiating", startTime, typeSchema);
		}
		
		protected synchronized void maybeLogComputedActivation(String action, double startTime, IChunk chunk) {
			if(currentAnaphorInfo != null
					&& currentAnaphorInfo.getDaia().equals("ia")
					&& !specifiedOrInstantiatedAnaphors.contains(currentAnaphorInfo.getId())) {
				specifiedOrInstantiatedAnaphors.add(currentAnaphorInfo.getId());
				logComputedActivation(action, currentAnaphorInfo.getId(), startTime, chunk);
			} // else: ignore direct anaphors
		}

		protected void logComputedActivation(String action, int anaphorId, double startTime, IChunk chunk) {
			double computedActivation = Double.NaN;
			if(chunk != null) {
				computedActivation = chunk.getSubsymbolicChunk().getActivation();
			}
			logWriter.println(String.format("%1$5.2f Specifying chunk for anaphor %2$2d:"
					+" computed activation=%3$4.3f for chunk=%4$s",
					startTime, anaphorId, computedActivation, chunk));
		}
		
		public synchronized void maybeLogActivationAtRegressionStart(double requestTime, IChunk chunk) {
			AnaphorInfo newInfo = (AnaphorInfo)chunk.getMetaData(ITWM.anaphorInfo);
			if(newInfo != null) {
				if(!seenAnaphors.contains(newInfo.getId())) {
					logRelationActivation(requestTime, newInfo);
				}
				seenAnaphors.add(newInfo.getId());
			}
			currentAnaphorInfo = newInfo;
		}
		
		protected void logRelationActivation(double requestTime, AnaphorInfo info) {
			UnderspecifiedRelation relation = underspecifiedRelations.get(info.getId());
			if(relation == null) {
				if(info.getDaia().equals("ia")) {
					throw new IllegalArgumentException("Unknown indirect anaphor id="+info.getId());
				} // else: direct anaphors are omitted
			} else if(!relation.estimatedActivationLevel.name().toLowerCase()
					.equals(info.getRelationActivationLevel())){
				throw new IllegalArgumentException("Relation levels do not match: "
					+relation.estimatedActivationLevel.name().toLowerCase()
					+" vs. "+info.getRelationActivationLevel());
			} else {
				try {
					IChunk defaultChunk = dm.getChunk(relation.defaultId).get();
					double computedActivation = Double.NaN;
					if(defaultChunk != null) {
						computedActivation = defaultChunk.getSubsymbolicChunk().getActivation();
					}
					// TODO: log normalized values
					logWriter.println(String.format("%1$5.2f On-line activation of anaphor %2$2d: "
							+" estimated: %3$4.3f (%4$s) computed: %5$4.3f for default=%6$s",
							requestTime, info.getId(), relation.estimatedActivation,
							relation.estimatedActivationLevel, computedActivation, relation.defaultId));
				} catch (InterruptedException|ExecutionException e) {
					LOGGER.error("Failed to get computed activation for "
							+relation.defaultId+": "+e.getMessage(), e);
				}
			}
		}
		
	}
	
	private static class UnderspecifiedRelation {
		
		private enum Level {
			
			HIGH, LOW;
			
			private static Level parseLevel(char level) {
				switch(level) {
				case 'H': return HIGH;
				case 'L': return LOW;
				default: throw new IllegalArgumentException("Unknown level: "+level);
				}
			}
			
		}
		
		private final double estimatedActivation;
		private final Level estimatedActivationLevel;
		private final String defaultId;
		
		public UnderspecifiedRelation(double estimatedActivation, char estimatedActivationLevel, String defaultId) {
			this(estimatedActivation, Level.parseLevel(estimatedActivationLevel), defaultId);
		}
		
		public UnderspecifiedRelation(double value, Level level, String defaultId) {
			this.estimatedActivation = value;
			this.estimatedActivationLevel = level;
			this.defaultId = defaultId;
		}

		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result
					+ ((defaultId == null) ? 0 : defaultId.hashCode());
			result = prime * result + ((estimatedActivationLevel == null) ? 0 : estimatedActivationLevel.hashCode());
			long temp;
			temp = Double.doubleToLongBits(estimatedActivation);
			result = prime * result + (int) (temp ^ (temp >>> 32));
			return result;
		}

		@Override
		public boolean equals(Object obj) {
			if (this == obj)
				return true;
			if (obj == null)
				return false;
			if (getClass() != obj.getClass())
				return false;
			UnderspecifiedRelation other = (UnderspecifiedRelation) obj;
			if (defaultId == null) {
				if (other.defaultId != null)
					return false;
			} else if (!defaultId.equals(other.defaultId))
				return false;
			if (estimatedActivationLevel != other.estimatedActivationLevel)
				return false;
			if (Double.doubleToLongBits(estimatedActivation) != Double
					.doubleToLongBits(other.estimatedActivation))
				return false;
			return true;
		}

		@Override
		public String toString() {
			return "UnderspecifiedRelation [value=" + estimatedActivation + ", level=" + estimatedActivationLevel
					+ ", defaultId=" + defaultId + "]";
		}
		
	}
}
