package de.monochromata.jactr.instruments.regressions;

import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.jactr.core.model.IModel;

import de.monochromata.jactr.instruments.fixations.FixationDurations;
import de.monochromata.jactr.remma.Fixation;
import de.monochromata.jactr.remma.Fixation.RegressionInfo;

public class RegressionPathDurations extends FixationDurations {

	private static final transient Log LOGGER = LogFactory.getLog(RegressionPathDurations.class);

	public static final String GENERATED_REGRESSION_PATHS_LOG_FILE_NAME = "generatedRegressionPathsLogFileName";
	public static final String AGGREGATED_REGRESSION_PATHS_LOG_FILE_NAME = "aggregatedRegressionPathsLogFileName";
	
	private PrintWriter aggregateWriter;

	@Override
	protected Collection<String> createPossibleParameters() {
		return Arrays.asList(new String[] {
				GENERATED_REGRESSION_PATHS_LOG_FILE_NAME,
				AGGREGATED_REGRESSION_PATHS_LOG_FILE_NAME,
				LOG_SEPARATOR
		});
	}

	@Override
	protected LoggingListener createListener() {
		String logFileName = getParameter(GENERATED_REGRESSION_PATHS_LOG_FILE_NAME);
		String aggregateLogFileName = getParameter(AGGREGATED_REGRESSION_PATHS_LOG_FILE_NAME);
		return new LoggingListener(getLogWriter(logFileName),
				getLogWriter(aggregateLogFileName,
						() -> { return aggregateWriter; },
						writer -> { aggregateWriter = writer; }),
				getSeparator());
	}
	
	@Override
	public void uninstall(IModel model) {
		super.uninstall(model);
		aggregateWriter.flush();
		aggregateWriter.close();
		aggregateWriter = null;
	}

	protected static class LoggingListener extends FixationDurations.LoggingListener {

		protected final PrintWriter aggregateWriter;
		protected List<GeneratedFixation> fixationsOnCurrentPath = new ArrayList<>();
		protected List<RegressionPath> iaRegressionPaths = new ArrayList<>();
		
		protected LoggingListener(PrintWriter writer,
				PrintWriter aggregateWriter,
				String separator) {
			super(writer, separator);
			this.aggregateWriter = aggregateWriter;
		}

		@Override
		public void fixationFinished(Fixation fixation, double start, double end) {
			if(!fixationsOnCurrentPath.isEmpty()) {
				if(!fixation.hasRegressionInfo()) {
					// Regression path ends
					finishRegressionPath();
				} else {
					// Regression path continues or is replaced by a new one
					if(fixation.getRegressionInfo().getPathId()
							!= fixationsOnCurrentPath.get(0).empiricalFixation
								.getRegressionInfo().getPathId()) {
						finishRegressionPath();
					}
					addToRegressionPath(fixation, start, end);
				}
			} else if(fixation.hasRegressionInfo()) {
				// Start a new regression path
				addToRegressionPath(fixation, start, end);
			} // else: there is no regression path and the fixation does not belong to a new one
		}

		private void addToRegressionPath(Fixation fixation, double start,
				double end) {
			fixationsOnCurrentPath.add(new GeneratedFixation(fixation, start, end));
		}

		private void finishRegressionPath() {
			computeAndLogRegression();
			fixationsOnCurrentPath.clear();
		}
		
		private void computeAndLogRegression() {
			if(fixationsOnCurrentPath.isEmpty())
				throw new IllegalStateException("Empty regression path");
			
			// Compute regression path duration
			LongCounter empiricalPathDurationMs = new LongCounter();
			LongCounter generatedPathDurationMs = new LongCounter();
			fixationsOnCurrentPath.forEach(fixation -> {
				empiricalPathDurationMs.count += fixation.empiricalFixation.getDurationMs();
				generatedPathDurationMs.count += (long)((fixation.generatedEnd-fixation.generatedStart)*1000);
			});
			
			// Log regression
			GeneratedFixation firstFixationOnPath = fixationsOnCurrentPath.get(0);
			Fixation empFix = firstFixationOnPath.empiricalFixation;
			RegressionInfo regressionInfo = empFix.getRegressionInfo();
			String aoiInfo = 
					separator+regressionInfo.getId()
					+separator+regressionInfo.getDaia()
					+separator+regressionInfo.getKind()
					+separator+regressionInfo.getRelationActivation();
			logDuration(LOGGER, aoiInfo, empFix,
					empFix.getStartTimestampMs(),
					empiricalPathDurationMs.count,
					firstFixationOnPath.generatedStart, generatedPathDurationMs.count,
					generatedPathDurationMs.count-empiricalPathDurationMs.count);
			
			// Save for IA regression paths for aggregation
			if(regressionInfo.getDaia().equals("ia")) {
				iaRegressionPaths.add(new RegressionPath(
						regressionInfo.getRelationActivation(),
						empFix.getCondition(),
						empiricalPathDurationMs.count,
						generatedPathDurationMs.count));
			}
		}
		
		private void aggregateIaRegressionPaths() {
			
			long highEmpiricalDurationMs = 0,
				 highGeneratedDurationMs = 0,
				 highCount			     = 0,
				 lowEmpiricalDurationMs  = 0,
				 lowGeneratedDurationMs  = 0,
				 lowCount				 = 0;
			
			// Sum
			for(RegressionPath path: iaRegressionPaths) {
				if(path.relationActivation.equals("high")) {
					highEmpiricalDurationMs += path.empiricalDurationMs;
					highGeneratedDurationMs += path.generatedDurationMs;
					highCount++;
				} else if(path.relationActivation.equals("low")) {
					lowEmpiricalDurationMs  += path.empiricalDurationMs;
					lowGeneratedDurationMs  += path.generatedDurationMs;
					lowCount++;
				} else {
					throw new IllegalArgumentException("Unknown relation activation: "+path.relationActivation);
				}
			}
			
			// Average
			highEmpiricalDurationMs = highCount==0?0:highEmpiricalDurationMs/highCount;
			highGeneratedDurationMs = highCount==0?0:highGeneratedDurationMs/highCount;
			lowEmpiricalDurationMs  = lowCount==0?0:lowEmpiricalDurationMs/lowCount;
			lowGeneratedDurationMs  = lowCount==0?0:lowGeneratedDurationMs/lowCount;
			
			// Print
			aggregateWriter.println("relationActivation"
					+separator+"empiricalDurationMs"
					+separator+"generatedDurationMs"
					+separator+"generated-empirical");
			aggregateWriter.println("high"
					+separator+highEmpiricalDurationMs
					+separator+highGeneratedDurationMs
					+separator+(highGeneratedDurationMs-highEmpiricalDurationMs));
			aggregateWriter.println("low"
					+separator+lowEmpiricalDurationMs
					+separator+lowGeneratedDurationMs
					+separator+(lowGeneratedDurationMs-lowEmpiricalDurationMs));
			aggregateWriter.println("low-high"
					+separator+(lowEmpiricalDurationMs-highEmpiricalDurationMs)
					+separator+(lowGeneratedDurationMs-highGeneratedDurationMs)
					+separator+((lowGeneratedDurationMs-highGeneratedDurationMs)
							   -(lowEmpiricalDurationMs-highEmpiricalDurationMs)));
			
			// TODO: Also print control and control:relationActivation,
			// but there might be data lacking for 
		}
		
		@Override
		protected void flush() {
			if(!fixationsOnCurrentPath.isEmpty()) {
				finishRegressionPath();
			}
			if(!iaRegressionPaths.isEmpty()) {
				aggregateIaRegressionPaths();
			}
		}
		
	}
	
	protected static class GeneratedFixation {
	
		private final Fixation empiricalFixation;
		private final double generatedStart,
						     generatedEnd;

		public GeneratedFixation(Fixation empiricalFixation,
				double generatedStart, double generatedEnd) {
			this.empiricalFixation = empiricalFixation;
			this.generatedStart = generatedStart;
			this.generatedEnd = generatedEnd;
		}
		
	}
		
	protected static class RegressionPath {
		
		private final String relationActivation;
		private final String condition;
		private final long empiricalDurationMs,
					 	   generatedDurationMs;
		
		public RegressionPath(String relationActivation, String condition,
				long empiricalDurationMs, long generatedDurationMs) {
			this.relationActivation = relationActivation;
			this.condition = condition;
			this.empiricalDurationMs = empiricalDurationMs;
			this.generatedDurationMs = generatedDurationMs;
		}
		
	}
	
	private static class LongCounter {
		
		private long count = 0;
		
	}
}
