package de.monochromata.jactr.instruments.activation;

import java.util.Arrays;
import java.util.Collection;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.jactr.core.model.IModel;
import org.jactr.core.utils.parameter.IParameterized;
import org.jactr.instrument.IInstrument;

import de.monochromata.jactr.instruments.AbstractInstrument;

public class FinalActivation extends AbstractInstrument {

	private static final transient Log LOGGER = LogFactory.getLog(FinalActivation.class);

	@Override
	protected Collection<String> createPossibleParameters() {
		return Arrays.asList(new String[] {});
	}
	
	// IInstrument implementation

	@Override
	public void install(IModel model) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void uninstall(IModel model) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void initialize() {
		// TODO Auto-generated method stub
		
	}

}
