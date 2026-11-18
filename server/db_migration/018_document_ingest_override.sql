-- ============================================================
-- Document-level segmentation parameter override:
--   Section 1: rename adi_knowledge_base.ingest_max_overlap -> ingest_max_overlap_size
--              (the old name mirrors upstream langchain4j maxOverlapSize incompletely;
--               with document-level override columns added in Section 2, both tables
--               keep a one-to-one same-name column contract)
--   Section 2: add the four document-level override columns on adi_document
--
-- Notes:
--   * Section 1 RENAME is not idempotent (PostgreSQL has no IF EXISTS for RENAME); run once.
--     Column comments survive the rename unchanged.
--   * Section 2 ADD COLUMN IF NOT EXISTS is idempotent.
--   * Existing documents default to "follow the knowledge base" on all four keys
--     (varchar '' / int -1 sentinels): upgrade is a zero-behavior-change operation.
-- ============================================================


-- ============================================================
-- Section 1: knowledge base column rename
-- Applicable database: PostgreSQL (the relational DB is always PostgreSQL)
-- ============================================================

ALTER TABLE adi_knowledge_base RENAME COLUMN ingest_max_overlap TO ingest_max_overlap_size;


-- ============================================================
-- Section 2: document-level override columns
-- Same names as the knowledge base columns; sentinel values mean "follow the
-- knowledge base": '' for the two varchar columns, -1 for the two int columns
-- (-1 is outside the valid range; overlap 0 is a legal override meaning
-- "no overlap", so 0 cannot serve as the sentinel).
-- ============================================================

ALTER TABLE adi_document
    ADD COLUMN IF NOT EXISTS ingest_split_strategy varchar(20) DEFAULT '' NOT NULL;

COMMENT ON COLUMN adi_document.ingest_split_strategy IS 'Document-level split-strategy override: empty = follow the knowledge base default; otherwise recursive/paragraph/line/sentence/custom';

ALTER TABLE adi_document
    ADD COLUMN IF NOT EXISTS ingest_max_segment_size int DEFAULT -1 NOT NULL;

COMMENT ON COLUMN adi_document.ingest_max_segment_size IS 'Document-level max segment size override in tokens: -1 = follow the knowledge base default';

ALTER TABLE adi_document
    ADD COLUMN IF NOT EXISTS ingest_max_overlap_size int DEFAULT -1 NOT NULL;

COMMENT ON COLUMN adi_document.ingest_max_overlap_size IS 'Document-level max overlap override in tokens: -1 = follow the knowledge base default; 0 = no overlap';

ALTER TABLE adi_document
    ADD COLUMN IF NOT EXISTS ingest_custom_separator varchar(100) DEFAULT '' NOT NULL;

COMMENT ON COLUMN adi_document.ingest_custom_separator IS 'Document-level custom separator override, only used when the effective strategy is custom; empty = follow the knowledge base default';

COMMENT ON COLUMN adi_document.index_version IS 'Generation of indexed artifacts built from this document; +1 on remark / segment_mode / split-param change (knowledge base defaults and document overrides; title and other metadata excluded). Index tasks snapshot it for staleness detection and merge-debounce';
