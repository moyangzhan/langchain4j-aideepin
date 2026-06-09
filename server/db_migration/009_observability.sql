-- ============================================================
-- Migration: Add observability metrics for workflow nodes & chat messages
-- Description:
--   1. Add duration and metrics columns to adi_workflow_runtime_node
--   2. Add input_tokens and output_tokens columns to adi_character_message
-- ============================================================

-- 1. Workflow Runtime Node Observability
ALTER TABLE adi_workflow_runtime_node
    ADD COLUMN IF NOT EXISTS duration INT DEFAULT 0,
    ADD COLUMN IF NOT EXISTS metrics JSONB DEFAULT '{}';

COMMENT ON COLUMN adi_workflow_runtime_node.duration IS '节点执行耗时（毫秒） | Node execution duration in ms';
COMMENT ON COLUMN adi_workflow_runtime_node.metrics IS '可观测指标 JSON：token 消耗、HTTP 状态码、搜索结果数等 | Observability metrics JSON';

-- 2. Chat Message Token Observability
ALTER TABLE adi_character_message
    ADD COLUMN IF NOT EXISTS input_tokens INT DEFAULT 0,
    ADD COLUMN IF NOT EXISTS output_tokens INT DEFAULT 0;

COMMENT ON COLUMN adi_character_message.input_tokens IS '输入 token 数量 | Input token count';
COMMENT ON COLUMN adi_character_message.output_tokens IS '输出 token 数量 | Output token count';

-- 3. Draw Duration Observability
ALTER TABLE adi_draw ADD COLUMN IF NOT EXISTS duration INT DEFAULT 0;
COMMENT ON COLUMN adi_draw.duration IS '生成耗时（毫秒） | Generation duration in ms';
