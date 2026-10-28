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

-- Rename knowledge-base QA tables/columns to the message naming, aligning with the
-- adi_character_message* family: chat = interaction layer (controllers/services/URLs),
-- message = data layer (entities/tables). qa_record_id becomes message_id, matching
-- adi_character_message_ref_*.message_id. The update-time trigger keeps its legacy
-- name when a table is renamed, so it is recreated under the new name.
-- No indexes exist on these tables.

alter table adi_knowledge_base_qa rename to adi_knowledge_base_message;
alter table adi_knowledge_base_qa_ref_embedding rename to adi_knowledge_base_message_ref_embedding;
alter table adi_knowledge_base_qa_ref_graph rename to adi_knowledge_base_message_ref_graph;

drop trigger trigger_kb_qa_update_time on adi_knowledge_base_message;
create trigger trigger_kb_message_update_time
    before update
    on adi_knowledge_base_message
    for each row
execute procedure update_modified_column();

alter table adi_knowledge_base_message_ref_embedding rename column qa_record_id to message_id;
alter table adi_knowledge_base_message_ref_graph rename column qa_record_id to message_id;
