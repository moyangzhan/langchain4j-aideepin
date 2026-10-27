-- Multi-instance workflow resume: persist the langgraph4j checkpoint at human-feedback
-- interruption so ANY instance can rebuild the engine and continue the run from PostgreSQL.
-- Idempotent: installations that already have the column are unchanged.

alter table adi_workflow_runtime
    add column if not exists checkpoint bytea;

COMMENT ON COLUMN adi_workflow_runtime.checkpoint IS 'Serialized langgraph4j checkpoint captured at human-feedback interruption; non-NULL only while status = 5 (waiting_input). Written by the same UPDATE that sets waiting_input; cleared at terminal status / timeout expiry';
