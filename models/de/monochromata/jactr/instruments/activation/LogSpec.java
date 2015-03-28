package de.monochromata.jactr.instruments.activation;

import org.jactr.core.chunk.IChunk;
import org.jactr.core.model.IModel;

/**
 * Log specifications are used to decide which chunks to log.
 */
public interface LogSpec {
	
	/**
	 * Decide whether the given chunk or all chunks reachable from it
	 * (transitively) via the slots should be logged.
	 * 
	 * @param chunk The chunk to be logged
	 * @param model The model that contains the chunk
	 * @return True, if the chunk should be logged, false otherwise
	 */
	public boolean shouldLog(IChunk chunk, IModel model);
}
