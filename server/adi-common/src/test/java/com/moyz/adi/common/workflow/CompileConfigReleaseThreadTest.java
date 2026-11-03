package com.moyz.adi.common.workflow;

import org.bsc.langgraph4j.CompileConfig;
import org.bsc.langgraph4j.CompiledGraph;
import org.bsc.langgraph4j.RunnableConfig;
import org.bsc.langgraph4j.StateGraph;
import org.bsc.langgraph4j.checkpoint.MemorySaver;
import org.bsc.langgraph4j.state.AgentState;
import org.junit.jupiter.api.Test;

import java.util.Map;

import static org.bsc.langgraph4j.StateGraph.END;
import static org.bsc.langgraph4j.StateGraph.START;
import static org.bsc.langgraph4j.action.AsyncNodeAction.node_async;
import static org.junit.jupiter.api.Assertions.*;

/**
 * Pins the checkpoint lifecycle WorkflowEngine relies on once a run reaches END:
 * the default CompileConfig releases the thread's checkpoints (a subsequent getState
 * throws "Missing Checkpoint!"), so compileApp must build with releaseThread(false) —
 * the engine reads the latest snapshot after the stream ends to tell WAITING_INPUT
 * apart from completion and to persist the interruption blob.
 */
class CompileConfigReleaseThreadTest {

    @Test
    void defaultCompileReleasesCheckpointsOnEnd() throws Exception {
        MemorySaver saver = new MemorySaver();
        CompiledGraph<AgentState> app = new StateGraph<AgentState>(AgentState::new)
                .addNode("only", node_async(state -> Map.of()))
                .addEdge(START, "only")
                .addEdge("only", END)
                .compile(CompileConfig.builder()
                        .checkpointSaver(saver)
                        .build());
        RunnableConfig config = RunnableConfig.builder().threadId("runtime-default").build();

        app.invoke(Map.of(), config);

        assertFalse(saver.get(config).isPresent());
        assertThrows(IllegalStateException.class, () -> app.getState(config));
    }

    @Test
    void releaseThreadDisabledKeepsCheckpointsReadableAfterEnd() throws Exception {
        MemorySaver saver = new MemorySaver();
        CompiledGraph<AgentState> app = new StateGraph<AgentState>(AgentState::new)
                .addNode("only", node_async(state -> Map.of()))
                .addEdge(START, "only")
                .addEdge("only", END)
                .compile(CompileConfig.builder()
                        .checkpointSaver(saver)
                        .releaseThread(false)
                        .build());
        RunnableConfig config = RunnableConfig.builder().threadId("runtime-held").build();

        app.invoke(Map.of(), config);

        assertTrue(saver.get(config).isPresent());
        assertNotNull(app.getState(config));
    }
}
