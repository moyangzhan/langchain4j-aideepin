-- ============================================================
-- Document-level ingest parameter override / 文档级分段参数覆盖
--
-- Purpose: allow each document to override the knowledge base ingest parameters
-- Main changes:
--   Section 1: rename adi_knowledge_base.ingest_max_overlap to ingest_max_overlap_size
--   Section 2: add four override columns on adi_document and refresh its index_version comment
--
-- ------------------------------------------------------------
--
-- 变更目的：支持按文档覆盖知识库的分段参数
-- 本次主要改动：
--   Section 1: 重命名 adi_knowledge_base.ingest_max_overlap 为 ingest_max_overlap_size
--   Section 2: 为 adi_document 新增 4 个分段参数覆盖列并更新 index_version 注释
--
-- ============================================================


-- ============================================================
-- Section 1: knowledge base column rename / 知识库列重命名
--
-- RENAME is not idempotent (PostgreSQL has no IF EXISTS for RENAME); run once.
--
-- ------------------------------------------------------------
--
-- RENAME 不具幂等性（PostgreSQL 的 RENAME 不支持 IF EXISTS），只能执行一次。
-- ============================================================

ALTER TABLE adi_knowledge_base RENAME COLUMN ingest_max_overlap TO ingest_max_overlap_size;


-- ============================================================
-- Section 2: document-level override columns / 文档级覆盖列
--
-- Same names as the knowledge base columns; an empty string (varchar columns) / -1 (int columns) means "follow the knowledge base".
--
-- ------------------------------------------------------------
--
-- 列名与知识库一致；varchar 列的空串、int 列的 -1 表示"跟随知识库"。
-- ============================================================

-- Step 2-1: add the four override columns.
ALTER TABLE adi_document
    ADD COLUMN IF NOT EXISTS ingest_split_strategy varchar(20) DEFAULT '' NOT NULL;
ALTER TABLE adi_document
    ADD COLUMN IF NOT EXISTS ingest_max_segment_size int DEFAULT -1 NOT NULL;
ALTER TABLE adi_document
    ADD COLUMN IF NOT EXISTS ingest_max_overlap_size int DEFAULT -1 NOT NULL;
ALTER TABLE adi_document
    ADD COLUMN IF NOT EXISTS ingest_custom_separator varchar(100) DEFAULT '' NOT NULL;

-- Step 2-2: set the column comments.
COMMENT ON COLUMN adi_document.ingest_split_strategy IS 'Document-level split-strategy override: empty = follow the knowledge base default; one of recursive/paragraph/line/sentence/custom';
COMMENT ON COLUMN adi_document.ingest_max_segment_size IS 'Document-level max segment size override in tokens: -1 = follow the knowledge base default';
COMMENT ON COLUMN adi_document.ingest_max_overlap_size IS 'Document-level max overlap override in tokens: -1 = follow the knowledge base default; 0 = no overlap';
COMMENT ON COLUMN adi_document.ingest_custom_separator IS 'Document-level custom separator override, only used when the effective strategy is custom; empty = follow the knowledge base default';
COMMENT ON COLUMN adi_document.index_version IS 'Generation of indexed artifacts built from this document; +1 on remark / segment_mode / split-param change (knowledge base defaults and document overrides; title and other metadata excluded). Index tasks snapshot it for staleness detection and merge-debounce';
