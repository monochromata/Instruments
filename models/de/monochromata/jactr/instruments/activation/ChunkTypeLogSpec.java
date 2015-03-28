package de.monochromata.jactr.instruments.activation;

import java.util.concurrent.ExecutionException;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.jactr.core.chunk.IChunk;
import org.jactr.core.chunktype.IChunkType;
import org.jactr.core.chunktype.ISymbolicChunkType;
import org.jactr.core.model.IModel;
import org.jactr.core.module.declarative.IDeclarativeModule;

/**
 * A log specification that permits chunks of a given chunk
 * type to be logged. Note that this includes chunks of the child
 * types of the given chunk type.
 * 
 * @see IChunk#isA(IChunkType)
 */
public class ChunkTypeLogSpec implements LogSpec {

	private static final transient Log LOGGER = LogFactory
			.getLog(ChunkTypeLogSpec.class);
	
	private String chunkTypeName;
	private IChunkType chunkType;
	
	private ChunkTypeLogSpec() {
	}
	
	private ChunkTypeLogSpec(String chunkTypeName) {
		this.chunkTypeName = chunkTypeName;
	}
	
	public String getChunkTypeName() {
		return chunkTypeName;
	}

	public void setChunkTypeName(String chunkTypeName) {
		this.chunkTypeName = chunkTypeName;
		this.chunkType = null;
	}

	/**
	 * Return the chunk type permitted by this log specification.
	 * 
	 * @return Null, if the chunk type has not been obtained from
	 * 	declarative memory, yet.
	 * @see #getChunkType(IModel)
	 * @see IDeclarativeModule#getChunkType(String)
	 */
	public IChunkType getChunkType() {
		return chunkType;
	}

	/**
	 * Return the chunk type permitted by this log specification.
	 * 
	 * @param model
	 * @return Null only if the chunk type was not available before
	 * 	the invocation of this method and could not be obtained
	 *  from declarative memory.
	 * @see IModel#getDeclarativeModule()
	 * @see IDeclarativeModule#getChunkType(String)
	 */
	public IChunkType getChunkType(IModel model) {
		if(chunkType == null) {
			try {
				chunkType = model.getDeclarativeModule()
						.getChunkType(getChunkTypeName()).get();
			} catch (InterruptedException | ExecutionException e) {
				LOGGER.error("Could not get chunk type "+getChunkTypeName()
						+" from declarative memory: "+e.getMessage(), e);
			}
		}
		return chunkType;
	}
	
	public void setChunkType(IChunkType chunkType) {
		this.chunkType = chunkType;
	}

	@Override
	public boolean shouldLog(IChunk chunk, IModel model) {
		return chunk.isA(getChunkType(model));
	}

	@Override
	public String toString() {
		return "chunk type=" + chunkTypeName;
	}

}
