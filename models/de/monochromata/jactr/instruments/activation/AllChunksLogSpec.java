package de.monochromata.jactr.instruments.activation;

import org.jactr.core.chunk.IChunk;
import org.jactr.core.model.IModel;

/**
 * A log specification that permits all non-null chunks to be logged
 */
public class AllChunksLogSpec implements LogSpec {

	@Override
	public boolean shouldLog(IChunk chunk, IModel model) {
		return chunk != null;
	}

	@Override
	public String toString() {
		return "all chunks";
	}
	
}
