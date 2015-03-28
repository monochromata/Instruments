package de.monochromata.jactr.instruments.activation;

import org.jactr.core.chunk.IChunk;
import org.jactr.core.model.IModel;

/**
 * Permits all chunks of a given name or with a name prefixed by a given name
 * and a dash (i.e. copies of the chunk), to be logged.
 */
public class ChunkLogSpec implements LogSpec {

	private String chunkName;
	
	public ChunkLogSpec() {
	}
	
	public ChunkLogSpec(String chunkName) {
		this.chunkName = chunkName;
	}
	
	public String getChunkName() {
		return chunkName;
	}

	public void setChunkName(String chunkName) {
		this.chunkName = chunkName;
	}

	@Override
	public boolean shouldLog(IChunk chunk, IModel model) {
		String template = getChunkName();
		String name = chunk.getSymbolicChunk().getName();
		return name.equals(template) || name.startsWith(template+"-");
	}

	@Override
	public String toString() {
		return "chunk=" + chunkName;
	}
	
}
