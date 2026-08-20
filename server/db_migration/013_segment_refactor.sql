-- ============================================================
-- Segment refactor:
--   Section 1: rename adi_knowledge_base_item -> adi_document; add document-level segment_mode
--   Section 2: adi_knowledge_base: add KB-level ingest_child_max_segment_size (parent-child child chunk size)
--   Section 3: new relational segment tables (adi_document_segment / _question / _child_chunk)
--   Section 4: backfill segments from existing pgvector KB embedding tables, then clear their text column
--
-- Notes:
--   * segment_mode is document-level ONLY; no KB-level segment_mode column exists (intentional).
--   * Section 1 RENAME is not idempotent (PostgreSQL has no IF EXISTS for RENAME); run once.
--   * Section 4 loops over all known suffixed KB embedding tables (to_regclass-guarded). It is
--     naturally idempotent: once "text" is cleared, subsequent runs insert nothing and update nothing.
--   * After this migration the KB vector table's "text" column is empty for migrated rows: the
--     relational segment tables are the single source of truth for segment content. For very large
--     vector tables the clear UPDATE can be run in batches manually; skipping it does not affect
--     correctness, it only leaves stale duplicate content in place.
--   * Vector-side segment columns (hit_count / word_count on adi_knowledge_base_embedding[_suffix])
--     are no longer maintained for the KB store; the app no longer calls ensureColumns() for it
--     (character memory tables keep theirs). Existing columns are left in place, harmless.
--   * neo4j vector backends: relational backfill for them is done by the app at startup
--     (SegmentNeo4jBackfillRunner), not by this script.
-- ============================================================


-- ============================================================
-- Section 1: adi_knowledge_base_item -> adi_document
-- Applicable database: PostgreSQL (the relational DB is always PostgreSQL)
-- ============================================================

ALTER TABLE adi_knowledge_base_item RENAME TO adi_document;

ALTER TRIGGER trigger_kb_item_update_time ON adi_document RENAME TO trigger_document_update_time;

ALTER TABLE adi_document
    ADD COLUMN IF NOT EXISTS segment_mode varchar(20) DEFAULT 'text' NOT NULL;

COMMENT ON TABLE  adi_document IS 'Knowledge Base Document';
COMMENT ON COLUMN adi_document.segment_mode IS 'Segment mode of this document: text | qa | parent_child. text: segments are chunks (vectorized). qa: document_segment rows are answers (not vectorized), questions live in adi_document_segment_question (vectorized). parent_child: document_segment rows are parent chunks (not vectorized), child chunks live in adi_document_segment_child_chunk (vectorized)';


-- ============================================================
-- Section 2: adi_knowledge_base - KB-level parent-child child chunk size
-- All ingest_* chunking knobs stay KB-level; segment_mode is the only document-level setting.
-- ============================================================

ALTER TABLE adi_knowledge_base
    ADD COLUMN IF NOT EXISTS ingest_child_max_segment_size int DEFAULT 200 NOT NULL;

COMMENT ON COLUMN adi_knowledge_base.ingest_child_max_segment_size IS 'Parent-child segment mode: max child chunk size in tokens. KB-level tuning knob, same level as the other ingest_* columns';


-- ============================================================
-- Section 3: relational segment tables (single source of truth for segment content)
--   * text mode    -> one row per chunk; content = chunk text; embedding_id NOT NULL (vectorized)
--   * qa mode      -> one row per answer; content = answer text; embedding_id NULL
--                     (questions are vectorized, stored in adi_document_segment_question)
--   * parent_child -> one row per parent chunk; content = parent text; embedding_id NULL
--                     (child chunks are vectorized, stored in adi_document_segment_child_chunk)
-- ============================================================

CREATE TABLE IF NOT EXISTS adi_document_segment
(
    id           bigserial primary key,
    uuid         varchar(32) default ''                 not null,
    kb_uuid      varchar(32) default ''                 not null,
    doc_uuid     varchar(32) default ''                 not null,
    position     int         default 0                  not null,
    content      text                                    not null,
    word_count   int         GENERATED ALWAYS AS (char_length(content)) STORED not null,
    hit_count    int         default 0                  not null,
    embedding_id varchar(64),
    source       varchar(20) default 'doc'              not null,
    create_time  timestamp   default CURRENT_TIMESTAMP  not null,
    update_time  timestamp   default CURRENT_TIMESTAMP  not null,
    is_deleted   boolean     default false              not null
);

COMMENT ON TABLE  adi_document_segment IS 'Document Segment (text chunk / QA answer / parent-child parent chunk) - single source of truth for segment content and metadata';
COMMENT ON COLUMN adi_document_segment.uuid IS 'Segment UUID (also used as graph textSegmentId when the segment is graph-indexed)';
COMMENT ON COLUMN adi_document_segment.kb_uuid IS 'Knowledge Base UUID';
COMMENT ON COLUMN adi_document_segment.doc_uuid IS 'Document UUID (adi_document.uuid)';
COMMENT ON COLUMN adi_document_segment.position IS 'Zero-based order of the segment inside the document';
COMMENT ON COLUMN adi_document_segment.content IS 'text: chunk text / qa: answer text / parent_child: parent chunk text (the content returned to the LLM after retrieval expansion)';
COMMENT ON COLUMN adi_document_segment.word_count IS 'Character count of the content (auto-computed by PostgreSQL: char_length(content))';
COMMENT ON COLUMN adi_document_segment.hit_count IS 'text: direct hits; qa/parent_child: propagated +1 when a linked question/child chunk is hit';
COMMENT ON COLUMN adi_document_segment.embedding_id IS 'Vector store entry id; NOT NULL only in text mode (answers and parent chunks are not vectorized)';
COMMENT ON COLUMN adi_document_segment.source IS 'Origin of the segment: doc (from document ingestion) | manual (created manually) | annotation (reserved for the future annotation feature)';
COMMENT ON COLUMN adi_document_segment.create_time IS 'Creation time';
COMMENT ON COLUMN adi_document_segment.update_time IS 'Last update time';
COMMENT ON COLUMN adi_document_segment.is_deleted IS 'Whether the record is soft-deleted';

CREATE UNIQUE INDEX IF NOT EXISTS uk_document_segment_uuid ON adi_document_segment (uuid);
CREATE INDEX IF NOT EXISTS idx_document_segment_doc ON adi_document_segment (doc_uuid, position);
CREATE INDEX IF NOT EXISTS idx_document_segment_embedding ON adi_document_segment (embedding_id);
CREATE INDEX IF NOT EXISTS idx_document_segment_kb ON adi_document_segment (kb_uuid);

DROP TRIGGER IF EXISTS trigger_document_segment_update_time ON adi_document_segment;
CREATE TRIGGER trigger_document_segment_update_time
    before update
    on adi_document_segment
    for each row
execute procedure update_modified_column();


CREATE TABLE IF NOT EXISTS adi_document_segment_question
(
    id                bigserial primary key,
    uuid              varchar(32) default ''                 not null,
    kb_uuid           varchar(32) default ''                 not null,
    doc_uuid          varchar(32) default ''                 not null,
    answer_segment_id bigint      default 0                  not null,
    position          int         default 0                  not null,
    content           text                                    not null,
    word_count        int         GENERATED ALWAYS AS (char_length(content)) STORED not null,
    hit_count         int         default 0                  not null,
    embedding_id      varchar(64),
    create_time       timestamp   default CURRENT_TIMESTAMP  not null,
    update_time       timestamp   default CURRENT_TIMESTAMP  not null,
    is_deleted        boolean     default false              not null
);

COMMENT ON TABLE  adi_document_segment_question IS 'QA-mode question of a document segment; the question text is vectorized, the answer lives in adi_document_segment';
COMMENT ON COLUMN adi_document_segment_question.uuid IS 'Question UUID';
COMMENT ON COLUMN adi_document_segment_question.kb_uuid IS 'Knowledge Base UUID';
COMMENT ON COLUMN adi_document_segment_question.doc_uuid IS 'Document UUID (adi_document.uuid)';
COMMENT ON COLUMN adi_document_segment_question.answer_segment_id IS 'Answer segment id (adi_document_segment.id); multiple questions may point to the same answer';
COMMENT ON COLUMN adi_document_segment_question.position IS 'Zero-based order of the question within its answer segment';
COMMENT ON COLUMN adi_document_segment_question.content IS 'Question original text (this is the content that gets vectorized)';
COMMENT ON COLUMN adi_document_segment_question.word_count IS 'Character count of the question (auto-computed by PostgreSQL: char_length(content))';
COMMENT ON COLUMN adi_document_segment_question.hit_count IS 'How many times this question was hit by vector retrieval';
COMMENT ON COLUMN adi_document_segment_question.embedding_id IS 'Vector store entry id';
COMMENT ON COLUMN adi_document_segment_question.create_time IS 'Creation time';
COMMENT ON COLUMN adi_document_segment_question.update_time IS 'Last update time';
COMMENT ON COLUMN adi_document_segment_question.is_deleted IS 'Whether the record is soft-deleted';

CREATE UNIQUE INDEX IF NOT EXISTS uk_document_segment_question_uuid ON adi_document_segment_question (uuid);
CREATE INDEX IF NOT EXISTS idx_document_segment_question_answer ON adi_document_segment_question (answer_segment_id, position);
CREATE INDEX IF NOT EXISTS idx_document_segment_question_doc ON adi_document_segment_question (doc_uuid);
CREATE INDEX IF NOT EXISTS idx_document_segment_question_embedding ON adi_document_segment_question (embedding_id);

DROP TRIGGER IF EXISTS trigger_document_segment_question_update_time ON adi_document_segment_question;
CREATE TRIGGER trigger_document_segment_question_update_time
    before update
    on adi_document_segment_question
    for each row
execute procedure update_modified_column();


CREATE TABLE IF NOT EXISTS adi_document_segment_child_chunk
(
    id                bigserial primary key,
    uuid              varchar(32) default ''                 not null,
    kb_uuid           varchar(32) default ''                 not null,
    doc_uuid          varchar(32) default ''                 not null,
    parent_segment_id bigint      default 0                  not null,
    position          int         default 0                  not null,
    content           text                                    not null,
    word_count        int         GENERATED ALWAYS AS (char_length(content)) STORED not null,
    hit_count         int         default 0                  not null,
    embedding_id      varchar(64),
    create_time       timestamp   default CURRENT_TIMESTAMP  not null,
    update_time       timestamp   default CURRENT_TIMESTAMP  not null,
    is_deleted        boolean     default false              not null
);

COMMENT ON TABLE  adi_document_segment_child_chunk IS 'Parent-child mode child chunk of a document segment; the child text is vectorized, the parent lives in adi_document_segment';
COMMENT ON COLUMN adi_document_segment_child_chunk.uuid IS 'Child chunk UUID';
COMMENT ON COLUMN adi_document_segment_child_chunk.kb_uuid IS 'Knowledge Base UUID';
COMMENT ON COLUMN adi_document_segment_child_chunk.doc_uuid IS 'Document UUID (adi_document.uuid)';
COMMENT ON COLUMN adi_document_segment_child_chunk.parent_segment_id IS 'Parent segment id (adi_document_segment.id); multiple children belong to one parent';
COMMENT ON COLUMN adi_document_segment_child_chunk.position IS 'Zero-based order of the child chunk within its parent segment';
COMMENT ON COLUMN adi_document_segment_child_chunk.content IS 'Child chunk original text (this is the content that gets vectorized)';
COMMENT ON COLUMN adi_document_segment_child_chunk.word_count IS 'Character count of the child chunk (auto-computed by PostgreSQL: char_length(content))';
COMMENT ON COLUMN adi_document_segment_child_chunk.hit_count IS 'How many times this child chunk was hit by vector retrieval';
COMMENT ON COLUMN adi_document_segment_child_chunk.embedding_id IS 'Vector store entry id';
COMMENT ON COLUMN adi_document_segment_child_chunk.create_time IS 'Creation time';
COMMENT ON COLUMN adi_document_segment_child_chunk.update_time IS 'Last update time';
COMMENT ON COLUMN adi_document_segment_child_chunk.is_deleted IS 'Whether the record is soft-deleted';

CREATE UNIQUE INDEX IF NOT EXISTS uk_document_segment_child_chunk_uuid ON adi_document_segment_child_chunk (uuid);
CREATE INDEX IF NOT EXISTS idx_document_segment_child_chunk_parent ON adi_document_segment_child_chunk (parent_segment_id, position);
CREATE INDEX IF NOT EXISTS idx_document_segment_child_chunk_doc ON adi_document_segment_child_chunk (doc_uuid);
CREATE INDEX IF NOT EXISTS idx_document_segment_child_chunk_embedding ON adi_document_segment_child_chunk (embedding_id);

DROP TRIGGER IF EXISTS trigger_document_segment_child_chunk_update_time ON adi_document_segment_child_chunk;
CREATE TRIGGER trigger_document_segment_child_chunk_update_time
    before update
    on adi_document_segment_child_chunk
    for each row
execute procedure update_modified_column();


-- ============================================================
-- Section 4: backfill segments from existing pgvector KB embedding tables, then clear their text
-- All existing data is text-mode. The loop covers the base table and every known suffixed variant;
-- to_regclass guards deployments that never created some of them. hit_count may be missing on the
-- vector table (added at app startup by ensureColumns); the expression degrades to 0 in that case.
-- ============================================================

DO $$
DECLARE
    vec_table text;
    vec_tables text[] := ARRAY[
        'adi_knowledge_base_embedding',
        'adi_knowledge_base_embedding_bge_384',
        'adi_knowledge_base_embedding_qwen_1024',
        'adi_knowledge_base_embedding_openai_1536'
        ];
    hit_count_expr text;
BEGIN
    FOREACH vec_table IN ARRAY vec_tables LOOP
        CONTINUE WHEN to_regclass(vec_table) IS NULL;

        IF EXISTS (SELECT 1 FROM information_schema.columns
                   WHERE table_schema = 'public' AND table_name = vec_table AND column_name = 'hit_count') THEN
            hit_count_expr := 'COALESCE(vec.hit_count, 0)';
        ELSE
            hit_count_expr := '0';
        END IF;

        -- 4a. Materialize every live vector row into a text-mode segment row.
        --     Idempotency: NOT EXISTS on embedding_id, plus rows with cleared text are filtered out.
        EXECUTE format(
            'INSERT INTO adi_document_segment (uuid, kb_uuid, doc_uuid, position, content, hit_count, embedding_id, source) ' ||
            'SELECT md5(random()::text || clock_timestamp()::text), ' ||
            '       COALESCE(vec.metadata ->> ''kb_uuid'', ''''), ' ||
            '       vec.metadata ->> ''kb_item_uuid'', ' ||
            '       (ROW_NUMBER() OVER (PARTITION BY vec.metadata ->> ''kb_item_uuid'' ORDER BY vec.embedding_id))::int, ' ||
            '       vec."text", ' ||
            '       ' || hit_count_expr || ', ' ||
            '       vec.embedding_id::text, ' ||
            '       ''doc'' ' ||
            'FROM %I AS vec ' ||
            'WHERE vec.metadata ->> ''kb_item_uuid'' IS NOT NULL ' ||
            '  AND vec.metadata ->> ''kb_item_uuid'' <> '''' ' ||
            '  AND vec."text" IS NOT NULL ' ||
            '  AND vec."text" <> '''' ' ||
            '  AND NOT EXISTS (SELECT 1 FROM adi_document_segment ds WHERE ds.embedding_id = vec.embedding_id::text)',
            vec_table
        );

        -- 4b. Clear the duplicated content: the relational layer is the single source of truth now.
        --     Only rows that were backfilled (i.e. carry kb_item_uuid metadata) are touched.
        --     For very large tables this UPDATE can be batched manually; skipping it is safe.
        EXECUTE format(
            'UPDATE %I AS vec SET "text" = '''' ' ||
            'WHERE vec.metadata ->> ''kb_item_uuid'' IS NOT NULL ' ||
            '  AND vec.metadata ->> ''kb_item_uuid'' <> '''' ' ||
            '  AND vec."text" IS NOT NULL ' ||
            '  AND vec."text" <> ''''',
            vec_table
        );
    END LOOP;
END $$;


-- ============================================================
-- Section 5: segment enable/disable + graph provenance ledger
--   * adi_document_segment: add is_enabled / enabled_change_time
--   * new adi_document_graph_vertex / adi_document_graph_edge:
--     one row per (graph element, segment) contribution - the authoritative
--     source for segment/doc-level graph cleanup and exclusivity judgement.
--     Element-level description/weight are aggregated on read from
--     per-segment fragments; the merged copy on graph vertices/edges is only
--     a retrieval cache. Legacy graph data is NOT backfilled: a document
--     joins the managed world by re-running graph extraction (lazy migration).
-- ============================================================

ALTER TABLE adi_document_segment
    ADD COLUMN IF NOT EXISTS is_enabled          boolean   DEFAULT true NOT NULL,
    ADD COLUMN IF NOT EXISTS enabled_change_time timestamp DEFAULT CURRENT_TIMESTAMP NOT NULL;

COMMENT ON COLUMN adi_document_segment.is_enabled          IS 'Whether this segment is enabled for retrieval (false = its vector & graph data has been deleted; enabling re-generates them)';
COMMENT ON COLUMN adi_document_segment.enabled_change_time IS 'Last enabled/disabled status change time';

ALTER TABLE adi_document_segment
    ADD COLUMN IF NOT EXISTS embedding_status int DEFAULT 3 NOT NULL,
    ADD COLUMN IF NOT EXISTS graphical_status int DEFAULT 3 NOT NULL;

COMMENT ON COLUMN adi_document_segment.embedding_status IS 'Rebuild status of this segment's vector data (segment-level, used by enable-segment async rebuild): 1=none (disabled), 2=rebuilding, 3=ready, 4=failed. Legacy rows default to 3';
COMMENT ON COLUMN adi_document_segment.graphical_status IS 'Rebuild status of this segment's graph data (segment-level, used by enable-segment async rebuild): 1=none (disabled), 2=rebuilding, 3=ready, 4=failed. Legacy rows default to 3';

CREATE TABLE IF NOT EXISTS adi_document_graph_vertex
(
    id           bigserial primary key,
    kb_uuid      varchar(32)  not null,
    doc_uuid     varchar(32)  not null,
    segment_uuid varchar(32)  not null,
    name         varchar(256) not null,
    entity_type  varchar(64),
    description  text,
    create_time  timestamp    default CURRENT_TIMESTAMP not null,
    CONSTRAINT uk_document_graph_vertex UNIQUE (kb_uuid, name, segment_uuid)
);

CREATE INDEX IF NOT EXISTS idx_dgv_segment ON adi_document_graph_vertex (segment_uuid);
CREATE INDEX IF NOT EXISTS idx_dgv_doc    ON adi_document_graph_vertex (doc_uuid);
CREATE INDEX IF NOT EXISTS idx_dgv_element ON adi_document_graph_vertex (kb_uuid, name);

COMMENT ON TABLE  adi_document_graph_vertex IS 'Graph vertex <-> document segment provenance: one row per (vertex, segment) contribution. Authoritative for segment/doc-level graph cleanup and exclusivity judgement';
COMMENT ON COLUMN adi_document_graph_vertex.kb_uuid      IS 'Knowledge Base UUID (element addressing scope: vertices are identified by name within a KB)';
COMMENT ON COLUMN adi_document_graph_vertex.doc_uuid     IS 'Document UUID of the contributing segment (denormalized from adi_document_segment for doc-level queries)';
COMMENT ON COLUMN adi_document_graph_vertex.segment_uuid IS 'Contributing segment uuid (adi_document_segment.uuid)';
COMMENT ON COLUMN adi_document_graph_vertex.name         IS 'Vertex entity name (same key GraphStoreIngestor uses to address vertices)';
COMMENT ON COLUMN adi_document_graph_vertex.entity_type  IS 'Entity type label from extraction (first-write wins, implicit endpoints null). Reserved for the future community-summary feature';
COMMENT ON COLUMN adi_document_graph_vertex.description  IS 'Entity description fragment extracted from THIS segment. Element-level description = concatenation of all contribution fragments (aggregated on read; never stored merged)';

CREATE TABLE IF NOT EXISTS adi_document_graph_edge
(
    id           bigserial primary key,
    kb_uuid      varchar(32)  not null,
    doc_uuid     varchar(32)  not null,
    segment_uuid varchar(32)  not null,
    source_name  varchar(256) not null,
    target_name  varchar(256) not null,
    description  text,
    weight       double precision default 1.0,
    create_time  timestamp    default CURRENT_TIMESTAMP not null,
    CONSTRAINT uk_document_graph_edge UNIQUE (kb_uuid, source_name, target_name, segment_uuid)
);

CREATE INDEX IF NOT EXISTS idx_dge_segment ON adi_document_graph_edge (segment_uuid);
CREATE INDEX IF NOT EXISTS idx_dge_doc    ON adi_document_graph_edge (doc_uuid);
CREATE INDEX IF NOT EXISTS idx_dge_element ON adi_document_graph_edge (kb_uuid, source_name, target_name);

COMMENT ON TABLE  adi_document_graph_edge IS 'Graph edge <-> document segment provenance: one row per (edge, segment) contribution';
COMMENT ON COLUMN adi_document_graph_edge.kb_uuid      IS 'Knowledge Base UUID';
COMMENT ON COLUMN adi_document_graph_edge.doc_uuid     IS 'Document UUID of the contributing segment';
COMMENT ON COLUMN adi_document_graph_edge.segment_uuid IS 'Contributing segment uuid (adi_document_segment.uuid)';
COMMENT ON COLUMN adi_document_graph_edge.source_name  IS 'Endpoint entity name A; canonically ordered with target_name by lexicographic order (smaller first) - NOT the extraction direction';
COMMENT ON COLUMN adi_document_graph_edge.target_name  IS 'Endpoint entity name B; canonically ordered with source_name by lexicographic order (larger second)';
COMMENT ON COLUMN adi_document_graph_edge.description  IS 'Relationship description fragment extracted from THIS segment; element-level description = concatenation of fragments';
COMMENT ON COLUMN adi_document_graph_edge.weight       IS 'Relationship strength given by THIS extraction; element-level weight = SUM over fragments';

-- ============================================================
-- Section 6: index version columns + index task queue
--   * index_version: generation of indexed artifacts; incremented whenever
--     existing index output is invalidated (remark / segment_mode / KB split
--     params / segment content edit; title and other metadata excluded).
--     Index tasks carry the version snapshot at enqueue time; the runner
--     conditionally finalizes (WHERE index_version = snapshot) and re-enqueues
--     the latest version on mismatch (merge-debounce).
--   * adi_index_task: scheduling source of truth for all index writes.
--     One row per (doc, segment, target, type); repeated enqueues only bump
--     the version of the pending row; same-doc tasks are serialized at claim
--     time (advisory lock), cross-doc tasks run in parallel.
-- ============================================================

ALTER TABLE adi_document_segment
    ADD COLUMN IF NOT EXISTS index_version int DEFAULT 0 NOT NULL;

COMMENT ON COLUMN adi_document_segment.index_version IS 'Generation of indexed artifacts built from this segment; +1 on segment content edit. Segment-level index tasks snapshot it for staleness detection';

ALTER TABLE adi_document
    ADD COLUMN IF NOT EXISTS index_version int DEFAULT 0 NOT NULL;

COMMENT ON COLUMN adi_document.index_version IS 'Generation of indexed artifacts built from this document; +1 on remark / segment_mode / KB split-param change (title and other metadata excluded). Index tasks snapshot it for staleness detection and merge-debounce';

CREATE TABLE IF NOT EXISTS adi_index_task
(
    id            bigserial primary key,
    kb_uuid       varchar(32)  not null,
    doc_uuid      varchar(32)  not null,
    user_id       bigint       not null,
    segment_uuid  varchar(32)  not null default '',
    target_type   varchar(20)  not null,
    task_type     varchar(20)  not null,
    index_version  int         not null,
    status        varchar(20)  not null,
    fail_reason   varchar(500),
    create_time   timestamp    default CURRENT_TIMESTAMP not null,
    update_time   timestamp    default CURRENT_TIMESTAMP not null,
    CONSTRAINT uk_index_task UNIQUE (doc_uuid, segment_uuid, target_type, task_type)
);

CREATE INDEX IF NOT EXISTS idx_index_task_status ON adi_index_task (status, id);
CREATE INDEX IF NOT EXISTS idx_index_task_doc ON adi_index_task (doc_uuid);

COMMENT ON TABLE  adi_index_task IS 'Index task queue: scheduling source of truth for all index writes (segmentation, embedding, graph extraction). One row per (doc_uuid, segment_uuid, target_type, task_type); document-level tasks use empty segment_uuid';
COMMENT ON COLUMN adi_index_task.kb_uuid      IS 'Owning knowledge base uuid (denormalized for fan-out enqueue and audit; not part of the merge key)';
COMMENT ON COLUMN adi_index_task.doc_uuid     IS 'Target document uuid (never empty)';
COMMENT ON COLUMN adi_index_task.user_id      IS 'Triggering user; async executors have no ThreadContext, billing context is persisted here';
COMMENT ON COLUMN adi_index_task.segment_uuid IS 'Target segment uuid for segment-level tasks; empty string for document-level tasks (PG unique constraints do not dedupe NULL)';
COMMENT ON COLUMN adi_index_task.target_type  IS 'document | segment';
COMMENT ON COLUMN adi_index_task.task_type    IS 'embedding | graphical';
COMMENT ON COLUMN adi_index_task.index_version IS 'Snapshot of the target''s index_version at enqueue time (same generation semantics as adi_document.index_version; avoids bare "version" which reads as optimistic-lock convention); updated on merge-upsert while pending';
COMMENT ON COLUMN adi_index_task.status       IS 'pending | running | done | failed (failed is manually retried by re-enqueue)';
COMMENT ON COLUMN adi_index_task.fail_reason  IS 'Truncated failure reason when status = failed';
COMMENT ON COLUMN adi_index_task.update_time  IS 'Also serves as claim heartbeat; running rows stale beyond 30 minutes are reset by the poller';
