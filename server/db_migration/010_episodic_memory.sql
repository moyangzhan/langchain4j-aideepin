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