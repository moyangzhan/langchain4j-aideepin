package com.moyz.adi.common.workflow;

import org.bsc.langgraph4j.RunnableConfig;
import org.bsc.langgraph4j.checkpoint.Checkpoint;
import org.junit.jupiter.api.Test;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Round-trip and behavior tests for the checkpoint persistence primitives: the blob codec
 * (StoredCheckpoint fields + serializer-exercised state map) and the seeded saver (seed visible via
 * get, later put calls accumulate on top).
 */
class WorkflowCheckpointCodecTest {

    @Test
    void blobRoundTripsStoredCheckpointAndStateMap() {
        Map<String, Object> state = new LinkedHashMap<>();
        state.put("name", "LLM 节点");
        state.put("next", "node-uuid-2");
        state.put("completeResult", "最终回答文本");
        state.put("listValue", List.of("a", "b"));
        Checkpoint checkpoint = Checkpoint.builder()
                .id("cp-1")
                .nodeId("node-uuid-1")
                .nextNodeId("node-uuid-2")
                .state(state)
                .build();

        byte[] blob = WorkflowCheckpointCodec.toBlob(checkpoint);
        assertNotNull(blob);
        assertTrue(blob.length > 0);

        Checkpoint restored = WorkflowCheckpointCodec.fromBlob(blob);
        assertEquals("cp-1", restored.getId());
        assertEquals("node-uuid-1", restored.getNodeId());
        assertEquals("node-uuid-2", restored.getNextNodeId());
        assertEquals("LLM 节点", restored.getState().get("name"));
        assertEquals("node-uuid-2", restored.getState().get("next"));
        assertEquals("最终回答文本", restored.getState().get("completeResult"));
        assertEquals(List.of("a", "b"), restored.getState().get("listValue"));
    }

    @Test
    void corruptedBlobFailsFastInsteadOfReturningGarbage() {
        byte[] blob = WorkflowCheckpointCodec.toBlob(Checkpoint.builder()
                .id("cp").nodeId("n").nextNodeId("x")
                .state(Map.of("next", "x"))
                .build());
        //Truncate mid-stream: Java serialization has no checksum, so a structural break is required
        byte[] truncated = java.util.Arrays.copyOf(blob, Math.max(1, blob.length / 2));
        assertThrows(IllegalStateException.class, () -> WorkflowCheckpointCodec.fromBlob(truncated));
    }
}

/**
 * The seeded saver must behave as a MemorySaver that already contains the deserialized
 * checkpoint: get() returns the seed, and put() during the resumed run accumulates on top
 * (a read-only holder would leave getState() stale at the next interruption).
 */
class PreloadedSaverTest {

    @Test
    void seedIsVisibleViaGetAndLaterPutsAccumulate() throws Exception {
        Checkpoint seed = Checkpoint.builder()
                .id("cp-seed")
                .nodeId("node-uuid-1")
                .nextNodeId("feedback-node")
                .state(Map.of("next", "feedback-node"))
                .build();
        RunnableConfig config = RunnableConfig.builder().threadId("runtime-uuid").build();
        PreloadedSaver saver = new PreloadedSaver(config, seed);

        Optional<Checkpoint> latest = saver.get(config);
        assertTrue(latest.isPresent());
        assertEquals("cp-seed", latest.get().getId());
        assertEquals("feedback-node", latest.get().getNextNodeId());

        Checkpoint next = Checkpoint.builder()
                .id("cp-after-feedback")
                .nodeId("feedback-node")
                .nextNodeId("node-uuid-3")
                .state(Map.of("next", "node-uuid-3"))
                .build();
        saver.put(config, next);

        assertEquals("cp-after-feedback", saver.get(config).orElseThrow().getId());
        assertEquals(2, saver.list(config).size(), "puts must accumulate, not replace the seed");
    }

    @Test
    void distinctThreadIdsKeepTheirOwnCheckpoints() throws Exception {
        Checkpoint seed = Checkpoint.builder()
                .id("cp-a").nodeId("n").nextNodeId("f")
                .state(Map.of("next", "f"))
                .build();
        RunnableConfig configA = RunnableConfig.builder().threadId("runtime-a").build();
        RunnableConfig configB = RunnableConfig.builder().threadId("runtime-b").build();
        PreloadedSaver saver = new PreloadedSaver(configA, seed);

        assertTrue(saver.get(configA).isPresent());
        assertFalse(saver.get(configB).isPresent(), "thread pinning must isolate runtimes");
    }
}
