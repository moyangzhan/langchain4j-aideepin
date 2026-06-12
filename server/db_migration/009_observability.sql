-- ============================================================
-- Migration: Add observability metrics for workflow nodes & LLM call records
-- Description:
--   1. Add duration and metrics columns to adi_workflow_runtime_node
--   2. Create adi_llm_call_record table to unify LLM token tracking across the site
--   3. Migrate existing token data from adi_character_message and adi_knowledge_base_qa
--   4. Deprecate old token columns in adi_character_message and adi_knowledge_base_qa
--   5. Add draw duration column
-- ============================================================

-- 1. Workflow Runtime Node Observability
ALTER TABLE adi_workflow_runtime_node
    ADD COLUMN IF NOT EXISTS duration INT DEFAULT 0,
    ADD COLUMN IF NOT EXISTS metrics JSONB DEFAULT '{}';

COMMENT ON COLUMN adi_workflow_runtime_node.duration IS '节点执行耗时（毫秒） | Node execution duration in ms';
COMMENT ON COLUMN adi_workflow_runtime_node.metrics IS 'Observability metrics JSON: token consumption, HTTP status code, search result counts, etc.';

-- 2. Create adi_llm_call_record table
CREATE TABLE adi_llm_call_record (
    id             bigserial PRIMARY KEY,
    uuid           varchar(32)  NOT NULL DEFAULT '',
    source_type    smallint     NOT NULL DEFAULT 0,     -- 0:unknown 1:character_chat 2:knowledge_base_qa 3:knowledge_base_ingest 4:workflow_node 5:agent
    source_id      bigint       NOT NULL DEFAULT 0,     -- Source record primary key
    user_id        bigint       NOT NULL DEFAULT 0,
    model_platform varchar(50)  NOT NULL DEFAULT '',
    model_name     varchar(100) NOT NULL DEFAULT '',
    input_tokens   integer      NOT NULL DEFAULT 0,
    output_tokens  integer      NOT NULL DEFAULT 0,
    duration       integer      NOT NULL DEFAULT 0,     --  Call duration in ms
    request_time   timestamp    NOT NULL DEFAULT CURRENT_TIMESTAMP,   -- Request start time
    create_time    timestamp    NOT NULL DEFAULT CURRENT_TIMESTAMP,    -- Record creation time
    update_time    timestamp    NOT NULL DEFAULT CURRENT_TIMESTAMP,    -- Last update time
    is_deleted     boolean      NOT NULL DEFAULT false                 -- Logical delete flag
);

CREATE INDEX idx_llm_call_record_source ON adi_llm_call_record(source_type, source_id);
CREATE INDEX idx_llm_call_record_user_id ON adi_llm_call_record(user_id);
CREATE INDEX idx_llm_call_record_request_time ON adi_llm_call_record(request_time);

COMMENT ON TABLE adi_llm_call_record IS 'Unified LLM call resource consumption tracking';
COMMENT ON COLUMN adi_llm_call_record.source_type IS '来源类型：0=unknown 1=character_chat 2=knowledge_base_qa 3=knowledge_base_ingest 4=workflow_node 5=agent';
COMMENT ON COLUMN adi_llm_call_record.source_id IS 'Source record primary key';
COMMENT ON COLUMN adi_llm_call_record.user_id IS 'User ID';
COMMENT ON COLUMN adi_llm_call_record.model_platform IS 'Model platform name';
COMMENT ON COLUMN adi_llm_call_record.model_name IS 'Model name';
COMMENT ON COLUMN adi_llm_call_record.input_tokens IS 'Input token count';
COMMENT ON COLUMN adi_llm_call_record.output_tokens IS 'Output token count';
COMMENT ON COLUMN adi_llm_call_record.duration IS 'Call duration in ms';
COMMENT ON COLUMN adi_llm_call_record.request_time IS 'Request start time';

-- 3. Migrate existing token data

-- 从聊天消息迁移 | Migrate from chat messages
INSERT INTO adi_llm_call_record (uuid, source_type, source_id, user_id, model_platform, model_name, input_tokens, output_tokens, request_time, create_time)
SELECT
    gen_random_uuid(),
    1,
    a.id,
    a.user_id,
    COALESCE(m.platform, ''),
    COALESCE(m.name, ''),
    COALESCE(q.input_tokens, 0),
    COALESCE(a.output_tokens, 0),
    a.create_time,
    a.create_time
FROM adi_character_message a
LEFT JOIN adi_character_message q ON q.id = a.parent_message_id
LEFT JOIN adi_ai_model m ON m.id = a.ai_model_id
WHERE a.message_role = 3
  AND a.parent_message_id > 0
  AND (COALESCE(q.input_tokens, 0) > 0 OR COALESCE(a.output_tokens, 0) > 0);

-- 从知识库 QA 迁移 | Migrate from knowledge base QA
INSERT INTO adi_llm_call_record (uuid, source_type, source_id, user_id, model_platform, model_name, input_tokens, output_tokens, request_time, create_time)
SELECT
    gen_random_uuid(),
    2,
    id,
    user_id,
    COALESCE(m.platform, ''),
    COALESCE(m.name, ''),
    COALESCE(prompt_tokens, 0),
    COALESCE(answer_tokens, 0),
    create_time,
    create_time
FROM adi_knowledge_base_qa
LEFT JOIN adi_ai_model m ON m.id = ai_model_id
WHERE COALESCE(prompt_tokens, 0) > 0 OR COALESCE(answer_tokens, 0) > 0;

-- 4. Deprecate old token columns in adi_character_message (now tracked in adi_llm_call_record)
COMMENT ON COLUMN adi_character_message.input_tokens IS '[DEPRECATED] Migrated to adi_llm_call_record';
COMMENT ON COLUMN adi_character_message.output_tokens IS '[DEPRECATED] Migrated to adi_llm_call_record';
COMMENT ON COLUMN adi_character_message.tokens IS '[DEPRECATED] Migrated to adi_llm_call_record';

-- 5. Deprecate old token columns in adi_knowledge_base_qa (now tracked in adi_llm_call_record)
COMMENT ON COLUMN adi_knowledge_base_qa.prompt_tokens IS '[DEPRECATED] Migrated to adi_llm_call_record';
COMMENT ON COLUMN adi_knowledge_base_qa.answer_tokens IS '[DEPRECATED] Migrated to adi_llm_call_record';

-- 6. Draw Duration Observability
ALTER TABLE adi_draw ADD COLUMN IF NOT EXISTS duration INT DEFAULT 0;
COMMENT ON COLUMN adi_draw.duration IS 'Generation duration in ms';
