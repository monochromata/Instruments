package de.monochromata.jactr.instruments.activation;

import java.beans.ExceptionListener;
import java.beans.XMLDecoder;
import java.beans.XMLEncoder;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Scanner;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

/**
 * The configuration is to be read using {@link XMLDecoder} and can be
 * written using {@link XMLEncoder}.
 */
public class Configuration {
	
	private static final transient Log LOGGER = LogFactory
			.getLog(Configuration.class);

	private int sampleDurationMs = 50;
	
	private List<LogSpec> logSpecs = new ArrayList<>();
	
	private int maxGraphDepth = 3;
	
	private List<String> buffersToGraph = new ArrayList<>();
	
	private List<String> buffersToTrackEventsFor = new ArrayList<>();
	
	private String outputFileName = "activationGraph.log";
	
	public Configuration() {
	}

	public int getSampleDurationMs() {
		return sampleDurationMs;
	}

	public void setSampleDurationMs(int sampleDurationMs) {
		this.sampleDurationMs = sampleDurationMs;
	}

	public List<LogSpec> getLogSpecs() {
		return logSpecs;
	}

	public void setLogSpecs(List<LogSpec> logSpecs) {
		this.logSpecs = logSpecs;
	}

	public int getMaxGraphDepth() {
		return maxGraphDepth;
	}

	public void setMaxGraphDepth(int maxGraphDepth) {
		this.maxGraphDepth = maxGraphDepth;
	}

	public List<String> getBuffersToGraph() {
		return buffersToGraph;
	}

	public void setBuffersToGraph(List<String> buffersToGraph) {
		this.buffersToGraph = buffersToGraph;
	}
	
	public List<String> getBuffersToTrackEventsFor() {
		return buffersToTrackEventsFor;
	}

	public void setBuffersToTrackEventsFor(List<String> buffersToTrackEventsFor) {
		this.buffersToTrackEventsFor = buffersToTrackEventsFor;
	}

	public String getOutputFileName() {
		return outputFileName;
	}

	public void setOutputFileName(String outputFileName) {
		this.outputFileName = outputFileName;
	}

	public static synchronized Configuration loadFromFile(String fileName) throws FileNotFoundException {
		if(new File(fileName).exists()) {
			XMLDecoder decoder = new XMLDecoder(new FileInputStream(fileName));
			decoder.setExceptionListener(new ExceptionListener() {
				@Override
				public void exceptionThrown(Exception e) {
					LOGGER.error("Failed to load configuration from "+fileName+": "+e.getMessage(), e);
				}});
			Configuration config = (Configuration)decoder.readObject();
			decoder.close();
			return config;
		} else {
			return null;
		}
	}
	
	public static synchronized void saveToFile(String fileName,
			Configuration config) throws IOException {
		File file = new File(fileName);
		file.getParentFile().mkdirs();
		XMLEncoder encoder = new XMLEncoder(new FileOutputStream(fileName));
		encoder.writeObject(config);
		encoder.close();
	}
	
	public static Configuration getDefault() {
		Configuration config = new Configuration();
		config.setLogSpecs(new ArrayList<>(Arrays.asList(new LogSpec[] {
			new AllChunksLogSpec()
		})));
		config.setBuffersToGraph(new ArrayList<>(Arrays.asList(new String[] { 
			"goal", "retrieval", "visual", "imaginal"
		})));
		config.setBuffersToTrackEventsFor(new ArrayList<>(Arrays.asList(new String[] {
			"visual", "retrieval"
		})));
		return config;
	}
}
