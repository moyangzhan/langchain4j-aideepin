package com.moyz.adi.common.workflow;

import org.bsc.langgraph4j.RunnableConfig;
import org.bsc.langgraph4j.checkpoint.Checkpoint;
import org.bsc.langgraph4j.checkpoint.MemorySaver;

/**
 * 预置 checkpoint 种子的 MemorySaver
 * <p>
 * MemorySaver seeded with a checkpoint deserialized from the runtime row.
 * Seeding goes through the normal {@code put()} path so subsequent {@code put()} calls during the resumed execution accumulate on top —
 * a read-only holder would leave {@code app.getState()} stale at the next interruption and break the "re-interruption overwrites the snapshot" lifecycle.
 * </p>
 * <p>
 * 预置的种子经正常 put() 路径写入，续跑期间框架的新 put() 照常累积在其上——
 * 只读持有者会让第二次中断时 getState() 拿到陈旧快照，破坏「再次中断自然覆盖」的生命周期。
 * </p>
 */
public class PreloadedSaver extends MemorySaver {

    public PreloadedSaver(RunnableConfig config, Checkpoint seed) {
        super();
        try {
            put(config, seed);
        } catch (Exception e) {
            throw new IllegalStateException("Preload checkpoint failed", e);
        }
    }
}
