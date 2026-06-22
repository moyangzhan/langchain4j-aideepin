-- Episodic memory table for character conversations.
-- Stores event-like, timeline-bound memories that are append-only.
-- Each row corresponds to one episodic "event" extracted from a conversation turn.
-- <p>
-- 角色情景记忆表。存储事件型、绑定时间线的记忆，仅追加不合并。
-- 每行对应一轮对话中提取的一条情景事件。
CREATE TABLE IF NOT EXISTS adi_character_episodic_memory (
    id                BIGSERIAL    PRIMARY KEY,
    uuid              VARCHAR(32)  NOT NULL UNIQUE,
    character_id      BIGINT       NOT NULL,
    user_id           BIGINT       NOT NULL,
    summary           TEXT         NOT NULL,                             -- 事件摘要 / event summary
    source_msg_id     BIGINT       DEFAULT NULL,                         -- 源消息 ID，关联 character_message.id
    event_type        VARCHAR(64)  DEFAULT 'general',                    -- 事件类型，LLM 判定 / event type
    importance        SMALLINT     DEFAULT 3 CHECK (importance BETWEEN 1 AND 5), -- 重要性 1-5
    embedding_id      VARCHAR(64)  NOT NULL,                             -- 向量库 embedding ID
    created_at        TIMESTAMP    NOT NULL,
    last_accessed_at  TIMESTAMP    DEFAULT NULL,
    hit_count         INT          DEFAULT 0,
    is_active         BOOLEAN      DEFAULT TRUE,
    is_deleted        BOOLEAN      DEFAULT FALSE
);

CREATE INDEX IF NOT EXISTS idx_cem_char_user_time ON adi_character_episodic_memory (character_id, user_id, created_at DESC);
CREATE INDEX IF NOT EXISTS idx_cem_event_type     ON adi_character_episodic_memory (event_type);
CREATE INDEX IF NOT EXISTS idx_cem_source_msg     ON adi_character_episodic_memory (source_msg_id);

-- ============================================================
-- wan2.7-image:替换文生图模型(全新 image-generation 接口范式,替代 wanx2.1-t2i-turbo)
-- <p>
-- wan2.7-image: replace text-to-image model (new image-generation API paradigm,
-- supersedes wanx2.1-t2i-turbo which only supports the legacy image-synthesis endpoint).
-- ============================================================
DELETE FROM adi_ai_model WHERE name = 'wanx2.1-t2i-turbo' AND platform = 'dashscope';
-- 幂等:开发阶段迁移可能重复执行,先清理 wan2.7-image 再插入(name 无 UNIQUE 约束)
DELETE FROM adi_ai_model WHERE name = 'wan2.7-image' AND platform = 'dashscope';

INSERT INTO adi_ai_model (name, title, type, platform, properties, is_enable)
VALUES ('wan2.7-image', 'Qwen-Image', 'image', 'dashscope',
        '{"max_images": 4, "sizes": [{"value":"1K","label":"1K · 1024×1024"},{"value":"2K","label":"2K · 2048×2048"}]}',
        false);
