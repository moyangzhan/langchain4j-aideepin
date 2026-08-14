-- ============================================================
-- Section 1: Add hit count, word count and enable/disable status columns to adi_knowledge_base_item.
-- Section 2: Rename ref-graph column graph_from_llm -> entities_from_question on the two
--   *_ref_graph provenance tables (aligns upgraded databases with all_ddl.sql and the entities).
-- Segment-level columns on adi_knowledge_base_embedding[_<suffix>] are owned by the app
-- (PgVectorEmbeddingStoreConfig.ensureColumns()), not this migration.
-- ============================================================


-- ============================================================
-- Section 1: adi_knowledge_base_item (document table) - add new columns
-- Applicable database: PostgreSQL (all users; the main relational DB is always PostgreSQL)
-- Independent of vector/graph store selection; must be executed in all deployment environments.
-- ============================================================

ALTER TABLE adi_knowledge_base_item
    ADD COLUMN IF NOT EXISTS embedding_hit_count int       DEFAULT 0,
    ADD COLUMN IF NOT EXISTS graph_hit_count     int       DEFAULT 0,
    ADD COLUMN IF NOT EXISTS is_enabled          boolean   DEFAULT true,
    ADD COLUMN IF NOT EXISTS enabled_change_time timestamp DEFAULT CURRENT_TIMESTAMP;

-- word_count is a STORED generated column (char_length(remark)), kept separate from
-- the plain columns above so the GENERATED clause is clear. Idempotent via IF NOT EXISTS.
ALTER TABLE adi_knowledge_base_item
    ADD COLUMN IF NOT EXISTS word_count int GENERATED ALWAYS AS (char_length(remark)) STORED;

COMMENT ON TABLE  adi_knowledge_base_item IS 'Knowledge Base Document';
COMMENT ON COLUMN adi_knowledge_base_item.title    IS 'Document Title';
COMMENT ON COLUMN adi_knowledge_base_item.brief    IS 'Document Brief';
COMMENT ON COLUMN adi_knowledge_base_item.remark   IS 'Document Content';
COMMENT ON COLUMN adi_knowledge_base_item.embedding_hit_count IS 'How many times this document was recalled via vector (embedding) retrieval';
COMMENT ON COLUMN adi_knowledge_base_item.graph_hit_count     IS 'How many times this document was recalled via graph retrieval';
COMMENT ON COLUMN adi_knowledge_base_item.word_count          IS 'Character count of the document content (auto-computed: char_length(remark))';
COMMENT ON COLUMN adi_knowledge_base_item.is_enabled          IS 'Whether the document is enabled for retrieval (false = its segments are excluded from vector/graph search)';
COMMENT ON COLUMN adi_knowledge_base_item.enabled_change_time IS 'Last enabled/disabled status change time';


-- ============================================================
-- Section 2: Rename ref-graph column graph_from_llm -> entities_from_question
-- The column that stores the entities/vertices parsed from the user's question was
-- renamed in all_ddl.sql but never carried into an incremental migration, so databases
-- upgraded from earlier versions still carry the old name `graph_from_llm` while the
-- entities (and all_ddl.sql) expect `entities_from_question`. Affects two provenance tables:
--   * adi_knowledge_base_qa_ref_graph   (entity already on the new name)
--   * adi_character_message_ref_graph   (entity updated alongside this migration)
-- Note: run once. PostgreSQL has no IF EXISTS for RENAME COLUMN, so re-running on an
-- already-migrated database will error on the missing graph_from_llm column.
-- ============================================================

ALTER TABLE adi_knowledge_base_qa_ref_graph RENAME COLUMN graph_from_llm TO entities_from_question;
ALTER TABLE adi_character_message_ref_graph RENAME COLUMN graph_from_llm TO entities_from_question;
