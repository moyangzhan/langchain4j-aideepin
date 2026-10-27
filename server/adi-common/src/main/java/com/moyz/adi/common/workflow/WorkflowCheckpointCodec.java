package com.moyz.adi.common.workflow;

import org.bsc.langgraph4j.checkpoint.Checkpoint;
import org.bsc.langgraph4j.serializer.std.ObjectStreamStateSerializer;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serial;
import java.io.Serializable;
import java.util.Map;

/**
 * langgraph4j checkpoint 与 adi_workflow_runtime.checkpoint 列的互转编解码器；
 * 类结构演进后存量 blob 可能反序列化失败，此时 {@link #fromBlob} 抛 IllegalStateException，由调用方将该运行置为终态失败。
 * <p>
 * Checkpoint &lt;-&gt; the runtime row's checkpoint blob: a Java-serialized StoredCheckpoint record {checkpointId, nodeId, nextNodeId}
 * plus the state map serialized by the same {@link ObjectStreamStateSerializer} configuration the engine wires into the state graph;
 * if class shapes evolve, existing blobs may fail to deserialize — {@link #fromBlob} throws IllegalStateException and the caller fails the run.
 * </p>
 */
public final class WorkflowCheckpointCodec {

    private static final ObjectStreamStateSerializer<WfNodeState> STATE_SERIALIZER =
            new ObjectStreamStateSerializer<>(WfNodeState::new);

    private WorkflowCheckpointCodec() {
    }

    /**
     * 镜像 Checkpoint 的定位三字段（langgraph4j 的 Checkpoint 不可序列化），state 携带已序列化的状态图字节。
     * Mirrors the checkpoint's position fields (langgraph4j's Checkpoint is not Serializable); state carries the serialized state-map bytes.
     */
    private record StoredCheckpoint(String checkpointId, String nodeId, String nextNodeId,
                                    byte[] state) implements Serializable {
        @Serial
        private static final long serialVersionUID = 1L;
    }

    /**
     * 序列化快照为列 blob | Serialize a checkpoint into the column blob.
     *
     * @throws IllegalStateException 序列化失败（状态图值不可序列化等） / serialization failed
     */
    public static byte[] toBlob(Checkpoint checkpoint) {
        try {
            byte[] stateBytes = STATE_SERIALIZER.dataToBytes(checkpoint.getState());
            StoredCheckpoint stored = new StoredCheckpoint(checkpoint.getId(), checkpoint.getNodeId(), checkpoint.getNextNodeId(), stateBytes);
            try (ByteArrayOutputStream bos = new ByteArrayOutputStream();
                 ObjectOutputStream oos = new ObjectOutputStream(bos)) {
                oos.writeObject(stored);
                oos.flush();
                return bos.toByteArray();
            }
        } catch (IOException e) {
            throw new IllegalStateException("Checkpoint serialization failed", e);
        }
    }

    /**
     * 从列 blob 反序列化快照 | Deserialize a checkpoint from the column blob.
     *
     * @throws IllegalStateException 反序列化失败（版本演进/损坏） / deserialization failed
     */
    public static Checkpoint fromBlob(byte[] blob) {
        try (ObjectInputStream ois = new ObjectInputStream(new ByteArrayInputStream(blob))) {
            StoredCheckpoint stored = (StoredCheckpoint) ois.readObject();
            Map<String, Object> state = STATE_SERIALIZER.dataFromBytes(stored.state());
            return Checkpoint.builder()
                    .id(stored.checkpointId())
                    .nodeId(stored.nodeId())
                    .nextNodeId(stored.nextNodeId())
                    .state(state)
                    .build();
        } catch (IOException | ClassNotFoundException e) {
            throw new IllegalStateException("Checkpoint deserialization failed", e);
        }
    }
}
