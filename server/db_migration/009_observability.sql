-- ============================================================
-- Migration: Add observability metrics for workflow runs / nodes & LLM call records
-- Description:
--   1. Add duration and metadata columns to adi_workflow_runtime_node
--   2. Add input_tokens / output_tokens / duration_ms columns to adi_workflow_runtime
--   3. Create adi_llm_call_record table to unify LLM token tracking across the site
--   4. Migrate existing token data from adi_character_message and adi_knowledge_base_qa
--   5. Deprecate old token columns in adi_character_message and adi_knowledge_base_qa
--   6. Add draw duration column
-- ============================================================

-- 1. Workflow Runtime Node Observability
ALTER TABLE adi_workflow_runtime_node
    ADD COLUMN IF NOT EXISTS duration INT DEFAULT 0,
    ADD COLUMN IF NOT EXISTS metadata JSONB DEFAULT '{}';

COMMENT ON COLUMN adi_workflow_runtime_node.duration IS 'Node execution duration in ms';
COMMENT ON COLUMN adi_workflow_runtime_node.metadata IS 'Per-node-type runtime metadata as JSON: token usage for LLM nodes, HTTP status code for HTTP nodes, search result counts for search nodes, etc.';

-- 2. Workflow Runtime aggregated stats (terminal snapshot)
ALTER TABLE adi_workflow_runtime
    ADD COLUMN IF NOT EXISTS input_tokens  INT NOT NULL DEFAULT 0,
    ADD COLUMN IF NOT EXISTS output_tokens INT NOT NULL DEFAULT 0,
    ADD COLUMN IF NOT EXISTS duration      INT NOT NULL DEFAULT 0;

COMMENT ON COLUMN adi_workflow_runtime.input_tokens IS 'Total input tokens aggregated from LLM-typed nodes (terminal snapshot, written at success / fail / waiting_input)';
COMMENT ON COLUMN adi_workflow_runtime.output_tokens IS 'Total output tokens aggregated from LLM-typed nodes (terminal snapshot)';
COMMENT ON COLUMN adi_workflow_runtime.duration IS 'Total run duration in milliseconds aggregated from all nodes (terminal snapshot)';

-- 3. Create adi_llm_call_record table
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
COMMENT ON COLUMN adi_llm_call_record.source_type IS 'Source type: 0=unknown 1=character_chat 2=knowledge_base_qa 3=knowledge_base_ingest 4=workflow_node 5=agent';
COMMENT ON COLUMN adi_llm_call_record.source_id IS 'Source record primary key';
COMMENT ON COLUMN adi_llm_call_record.user_id IS 'User ID';
COMMENT ON COLUMN adi_llm_call_record.model_platform IS 'Model platform name';
COMMENT ON COLUMN adi_llm_call_record.model_name IS 'Model name';
COMMENT ON COLUMN adi_llm_call_record.input_tokens IS 'Input token count';
COMMENT ON COLUMN adi_llm_call_record.output_tokens IS 'Output token count';
COMMENT ON COLUMN adi_llm_call_record.duration IS 'Call duration in ms';
COMMENT ON COLUMN adi_llm_call_record.request_time IS 'Request start time';

-- 4. Migrate existing token data

-- Migrate from chat messages
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

-- Migrate from knowledge base QA
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

-- 5. Deprecate old token columns in adi_character_message (now tracked in adi_llm_call_record)
COMMENT ON COLUMN adi_character_message.input_tokens IS '[DEPRECATED] Migrated to adi_llm_call_record';
COMMENT ON COLUMN adi_character_message.output_tokens IS '[DEPRECATED] Migrated to adi_llm_call_record';
COMMENT ON COLUMN adi_character_message.tokens IS '[DEPRECATED] Migrated to adi_llm_call_record';

-- 6. Deprecate old token columns in adi_knowledge_base_qa (now tracked in adi_llm_call_record)
COMMENT ON COLUMN adi_knowledge_base_qa.prompt_tokens IS '[DEPRECATED] Migrated to adi_llm_call_record';
COMMENT ON COLUMN adi_knowledge_base_qa.answer_tokens IS '[DEPRECATED] Migrated to adi_llm_call_record';

-- 7. Draw Duration Observability
ALTER TABLE adi_draw ADD COLUMN IF NOT EXISTS duration INT DEFAULT 0;
COMMENT ON COLUMN adi_draw.duration IS 'Generation duration in ms';

-- 8. Add Agent workflow component (idempotent on name, bilingual by server locale)
INSERT INTO adi_workflow_component(uuid, name, title, remark, display_order, is_enable)
SELECT replace(gen_random_uuid()::text, '-', ''), 'Agent', 'Agent',
        'Invoke a Character with full capabilities: system prompt, knowledge base RAG, MCP tools, memory, web search',
        3, true
WHERE NOT EXISTS (SELECT 1 FROM adi_workflow_component WHERE name = 'Agent');
