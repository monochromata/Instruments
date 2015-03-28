package de.monochromata.jactr.instruments;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Consumer;
import java.util.function.Supplier;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.jactr.core.runtime.ACTRRuntime;
import org.jactr.core.utils.parameter.IParameterized;
import org.jactr.instrument.IInstrument;

public abstract class AbstractInstrument implements IInstrument, IParameterized {

	private static final transient Log LOGGER = LogFactory.getLog(AbstractInstrument.class);
	
	private final Map<String,String> parameterMap = new HashMap<>();
	private Collection<String> possibleParameters;

	public AbstractInstrument() {
		super();
	}
	
	protected abstract Collection<String> createPossibleParameters();

	@Override
	public Collection<String> getSetableParameters() {
		return getPossibleParameters();
	}

	@Override
	public Collection<String> getPossibleParameters() {
		if(possibleParameters == null) {
			possibleParameters = createPossibleParameters();
		}
		return possibleParameters;
	}

	@Override
	public void setParameter(String key, String value) {
		if(getPossibleParameters().contains(key)) {
			parameterMap.put(key, value);
		} else {
			throw new IllegalArgumentException("Unknown parameter: "+key);
		}
	}

	@Override
	public String getParameter(String key) {
		if(getPossibleParameters().contains(key)) {
			return parameterMap.get(key);
		} else {
			throw new IllegalArgumentException("Unknown parameter: "+key);
		}
	}

	
	protected PrintWriter getLogWriter(String logFileName, Supplier<PrintWriter> supplier,
			Consumer<PrintWriter> consumer) {
		if(supplier.get() == null) {
			try {
				consumer.accept(new PrintWriter(new BufferedWriter(new FileWriter(new File(
								ACTRRuntime.getRuntime().getWorkingDirectory(),
								logFileName)))));
			} catch (IOException e) {
				LOGGER.error("Failed to file: "+e.getMessage(), e);
			}
		}
		return supplier.get();
	}
}