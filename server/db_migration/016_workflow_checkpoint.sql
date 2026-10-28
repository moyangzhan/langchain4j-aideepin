-- Multi-instance workflow resume: persist the langgraph4j checkpoint at human-feedback
-- interruption so ANY instance can rebuild the engine and continue the run from PostgreSQL.
-- Idempotent: installations that already have the column are unchanged.

alter table adi_workflow_runtime
    add column if not exists checkpoint bytea;

COMMENT ON COLUMN adi_workflow_runtime.checkpoint IS 'Serialized langgraph4j checkpoint captured at human-feedback interruption; non-NULL only while status = 5 (waiting_input). Written by the same UPDATE that sets waiting_input; cleared at terminal status / timeout expiry';

-- Index task user-facing failure message: adi_index_task.user_msg stores a locale-independent
-- code with optional parameters ("INDEX_FAIL_MODEL_CALL|model-name" or "RAW|first-line-snippet");
-- rendering by user locale happens at read time via the i18n message bundles (FailReasonConverter /
-- listDocumentFailures). fail_reason keeps the raw exception for troubleshooting; legacy rows stay
-- NULL and are converted on the fly at read time. Idempotent.
alter table adi_index_task
    add column if not exists user_msg varchar(500) default '';

comment on column adi_index_task.user_msg is 'Locale-independent failure code with optional params (CODE|arg), rendered per user locale at read time; empty string = not converted yet (legacy rows / reruns), read side converts on the fly';
