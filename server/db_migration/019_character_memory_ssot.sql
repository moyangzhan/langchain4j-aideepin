-- ============================================================
-- Character memory content SSOT / 角色记忆内容 SSOT
--
-- Purpose: add instruction memory and make the data in the PostgreSQL memory tables the single source of truth for memory content
-- Main changes:
--   Section 1: 3 new relational memory tables in PostgreSQL (episodic / semantic / instruction)
--   Section 2: refresh the ref table's memory_type routing comment
--   Section 3: data migration when pgvector is used as the memory vector store (optional)
--   Section 4: data migration when neo4j is used as the memory vector store (optional)
--
-- ------------------------------------------------------------
--
-- 变更目的：新增指令型记忆及将 postgresql 中的记忆表中的数据做为记忆内容的单一事实源
-- 本次主要改动：
--   Section 1: 新增 3 张 PostgreSQL 记忆表（情景 / 语义 / 指令）
--   Section 2: 更新 ref 表 memory_type 的路由注释
--   Section 3: 使用 pgvector 做为记忆向量库的情况下的数据迁移（可选）
--   Section 4: 使用 neo4j 做为记忆向量库的情况下的数据迁移（可选）
--
-- ============================================================


-- ============================================================
-- Section 1: relational memory tables
-- ============================================================

CREATE TABLE IF NOT EXISTS adi_character_memory_episodic
(
    id            bigserial primary key,
    uuid          varchar(32) default ''               not null,
    character_id  bigint      default 0                not null,
    content       text        default ''               not null,
    event_type    varchar(20) default ''               not null,
    importance    int         default 3                not null,
    source_msg_id bigint      default -1               not null,
    user_id       bigint      default -1               not null,
    hit_count     int         default 0                not null,
    word_count    int         GENERATED ALWAYS AS (char_length(content)) STORED not null,
    create_time   timestamp   default CURRENT_TIMESTAMP not null,
    update_time   timestamp   default CURRENT_TIMESTAMP not null,
    is_deleted    boolean     default false            not null
);

COMMENT ON TABLE  adi_character_memory_episodic IS 'Episodic character memory (append-only timeline events) - single source of truth for memory content and structured fields';
COMMENT ON COLUMN adi_character_memory_episodic.uuid IS 'Memory UUID (UUID v7, 32 chars without dashes). The vector table primary key is this value in canonical 36-char dashed form; join key across vector/graph derived layers';
COMMENT ON COLUMN adi_character_memory_episodic.character_id IS 'Owning character id (adi_character.id); timeline and cleanup enumeration key';
COMMENT ON COLUMN adi_character_memory_episodic.content IS 'Memory content of the event';
COMMENT ON COLUMN adi_character_memory_episodic.event_type IS 'Event type code (see EventType enum)';
COMMENT ON COLUMN adi_character_memory_episodic.importance IS 'Importance clamped to 1-5 at write time';
COMMENT ON COLUMN adi_character_memory_episodic.source_msg_id IS 'Source character message id for traceability';
COMMENT ON COLUMN adi_character_memory_episodic.user_id IS 'Owner user id (character owner), resolved at write time';
COMMENT ON COLUMN adi_character_memory_episodic.hit_count IS 'Retrieval hit count (incremented on recall)';
COMMENT ON COLUMN adi_character_memory_episodic.word_count IS 'Character count of the content';
COMMENT ON COLUMN adi_character_memory_episodic.create_time IS 'Event creation time';
COMMENT ON COLUMN adi_character_memory_episodic.update_time IS 'Last update time';
COMMENT ON COLUMN adi_character_memory_episodic.is_deleted IS 'Whether the record is soft-deleted (deletion removes the vector row first, then soft-deletes this row)';

CREATE UNIQUE INDEX IF NOT EXISTS uk_character_memory_episodic_uuid ON adi_character_memory_episodic (uuid);
CREATE INDEX IF NOT EXISTS idx_character_memory_episodic_character_time ON adi_character_memory_episodic (character_id, create_time);

DROP TRIGGER IF EXISTS trigger_character_memory_episodic_update_time ON adi_character_memory_episodic;
CREATE TRIGGER trigger_character_memory_episodic_update_time
    before update
    on adi_character_memory_episodic
    for each row
execute procedure update_modified_column();


CREATE TABLE IF NOT EXISTS adi_character_memory_semantic
(
    id           bigserial primary key,
    uuid         varchar(32) default ''               not null,
    character_id bigint      default 0                not null,
    content      text        default ''               not null,
    user_id      bigint      default -1               not null,
    hit_count    int         default 0                not null,
    word_count   int         GENERATED ALWAYS AS (char_length(content)) STORED not null,
    create_time  timestamp   default CURRENT_TIMESTAMP not null,
    update_time  timestamp   default CURRENT_TIMESTAMP not null,
    is_deleted   boolean     default false            not null
);

COMMENT ON TABLE  adi_character_memory_semantic IS 'Semantic character memory (mergeable facts about the user) - single source of truth for memory content';
COMMENT ON COLUMN adi_character_memory_semantic.uuid IS 'Memory UUID (UUID v7, 32 chars without dashes). The vector table primary key is this value in canonical 36-char dashed form; join key across vector/graph derived layers';
COMMENT ON COLUMN adi_character_memory_semantic.character_id IS 'Owning character id (adi_character.id)';
COMMENT ON COLUMN adi_character_memory_semantic.content IS 'Memory content; one fact = one row';
COMMENT ON COLUMN adi_character_memory_semantic.user_id IS 'Owner user id (character owner), resolved at write time';
COMMENT ON COLUMN adi_character_memory_semantic.hit_count IS 'Retrieval hit count (incremented on recall)';
COMMENT ON COLUMN adi_character_memory_semantic.word_count IS 'Character count of the content';
COMMENT ON COLUMN adi_character_memory_semantic.create_time IS 'Creation time; backfilled rows take the migration time';
COMMENT ON COLUMN adi_character_memory_semantic.update_time IS 'Last update time';
COMMENT ON COLUMN adi_character_memory_semantic.is_deleted IS 'Whether the record is soft-deleted (deletion removes the vector row first, then soft-deletes this row)';

CREATE UNIQUE INDEX IF NOT EXISTS uk_character_memory_semantic_uuid ON adi_character_memory_semantic (uuid);
CREATE INDEX IF NOT EXISTS idx_character_memory_semantic_character ON adi_character_memory_semantic (character_id);

DROP TRIGGER IF EXISTS trigger_character_memory_semantic_update_time ON adi_character_memory_semantic;
CREATE TRIGGER trigger_character_memory_semantic_update_time
    before update
    on adi_character_memory_semantic
    for each row
execute procedure update_modified_column();


CREATE TABLE IF NOT EXISTS adi_character_memory_instruction
(
    id            bigserial primary key,
    uuid          varchar(32) default ''               not null,
    character_id  bigint      default 0                not null,
    content       text        default ''               not null,
    source_msg_id bigint      default -1               not null,
    user_id       bigint      default -1               not null,
    word_count    int         GENERATED ALWAYS AS (char_length(content)) STORED not null,
    create_time   timestamp   default CURRENT_TIMESTAMP not null,
    update_time   timestamp   default CURRENT_TIMESTAMP not null,
    is_deleted    boolean     default false            not null
);

COMMENT ON TABLE  adi_character_memory_instruction IS 'Instruction character memory (mergeable behaviour rules for the assistant) - single source of truth for memory content';
COMMENT ON COLUMN adi_character_memory_instruction.uuid IS 'Memory UUID (UUID v7, 32 chars without dashes)';
COMMENT ON COLUMN adi_character_memory_instruction.character_id IS 'Owning character id (adi_character.id); rules are scoped per user x character';
COMMENT ON COLUMN adi_character_memory_instruction.content IS 'Memory content; one rule = one row';
COMMENT ON COLUMN adi_character_memory_instruction.source_msg_id IS 'Source character message id (the turn the rule was learned from)';
COMMENT ON COLUMN adi_character_memory_instruction.user_id IS 'Owner user id (character owner), resolved at write time';
COMMENT ON COLUMN adi_character_memory_instruction.word_count IS 'Character count of the content';
COMMENT ON COLUMN adi_character_memory_instruction.create_time IS 'Creation time';
COMMENT ON COLUMN adi_character_memory_instruction.update_time IS 'Last update time';
COMMENT ON COLUMN adi_character_memory_instruction.is_deleted IS 'Whether the record is soft-deleted';

CREATE UNIQUE INDEX IF NOT EXISTS uk_character_memory_instruction_uuid ON adi_character_memory_instruction (uuid);
CREATE INDEX IF NOT EXISTS idx_character_memory_instruction_character ON adi_character_memory_instruction (character_id);

DROP TRIGGER IF EXISTS trigger_character_memory_instruction_update_time ON adi_character_memory_instruction;
CREATE TRIGGER trigger_character_memory_instruction_update_time
    before update
    on adi_character_memory_instruction
    for each row
execute procedure update_modified_column();


-- ============================================================
-- Section 2: refresh the memory-type routing comment on the ref table (comment-only, no data change)
-- ============================================================

COMMENT ON COLUMN adi_character_message_ref_memory_embedding.memory_type IS
    'Routing code: 1=semantic, 2=episodic, 3=instruction';


-- ============================================================
-- Section 3: data migration when pgvector is used as the memory vector store (optional) / 使用 pgvector 做为记忆向量库的情况下的数据迁移（可选）
--
-- !! NOT RUNNABLE AS-IS: replace the two placeholders first (search & replace, the whole "{name:default}" token) !!
--   Each placeholder embeds the default-install table name after the colon; a non-default embedding model means a different suffix
--   (openai example: adi_character_memory_embedding_openai_1536 / adi_character_episodic_memory_embedding_openai_1536).
--   The two placeholders point to two DIFFERENT physical tables — never replace both with the same table name.
--   {ACTIVE_SEMANTIC_EMBEDDING_TABLE:adi_character_memory_embedding_bge_512}
--   {ACTIVE_EPISODIC_EMBEDDING_TABLE:adi_character_episodic_memory_embedding_bge_512}
--
-- Active table name = base name + "_" + suffix, where suffix = <platform>_<dim> of the embedding model currently enabled in the admin console (openai_1536, qwen_1024; local bge-small-zh-v1.5 = bge_512; all-minilm-l6-v2 = bare base name).
-- List candidates:
--       SELECT table_name FROM information_schema.tables
--       WHERE table_schema = 'public'
--         AND table_name LIKE 'adi_character_%_memory_embedding%'
--       ORDER BY table_name;
--
-- Scope: ONLY the active tables are backfilled.
--
-- Re-run safety: rows already written by the app's double-write are skipped and win (their content is the newer one after any in-between UPDATE).
--
-- hit_count: if your active table lacks the column, replace every hit_count reference in 3-1/3-2 with 0.
--
-- neo4j vector backends: memory vectors live in the graph store, not in PostgreSQL; see Section 4.
--
-- ------------------------------------------------------------
--
-- ！！不能直接执行：先替换下面两个占位符（查找替换，整段 "{名称:默认值}" 一起替换）！！
--   每个占位符冒号后内嵌的是默认安装的表名；非默认 embedding 模型后缀不同
--   （openai 示例：adi_character_memory_embedding_openai_1536 / adi_character_episodic_memory_embedding_openai_1536）。
--   两个占位符指向两张不同的物理表——绝不可替换成同一张表名。
--   {ACTIVE_SEMANTIC_EMBEDDING_TABLE:adi_character_memory_embedding_bge_512}
--   {ACTIVE_EPISODIC_EMBEDDING_TABLE:adi_character_episodic_memory_embedding_bge_512}
-- 激活表名 = 基础名 + "_" + 后缀；后缀 = 管理后台当前启用的 embedding 模型的 <平台>_<维度>（openai_1536、qwen_1024；本地 bge-small-zh-v1.5 为 bge_512；all-minilm-l6-v2 无后缀）。
-- 候选表：
--       SELECT table_name FROM information_schema.tables
--       WHERE table_schema = 'public'
--         AND table_name LIKE 'adi_character_%_memory_embedding%'
--       ORDER BY table_name;
-- 范围：只回迁当前激活的表。
-- 可重复执行：应用双写已写入的行会被跳过并保留（期间发生 UPDATE 时，应用行的内容更新）。
-- hit_count：若激活表缺少该列，把 3-1/3-2 中的 hit_count 引用全部改为 0。
-- neo4j 向量后端：记忆向量在图存储而非 PostgreSQL 中；见 Section 4。
-- ============================================================

-- Step 3-1: backfill episodic rows from the episodic vector store.
INSERT INTO adi_character_memory_episodic
    (uuid, character_id, content, event_type, importance, source_msg_id, user_id, hit_count, create_time)
SELECT replace(u.embedding_id::text, '-', ''),
       (u.metadata ->> 'conv_id')::bigint,
       u."text",
       COALESCE(u.metadata ->> 'event_type', ''),
       COALESCE((u.metadata ->> 'importance')::int, 3),
       COALESCE((u.metadata ->> 'conv_msg_id')::bigint, -1),
       COALESCE(ch.user_id, -1),
       COALESCE(u.hit_count, 0),
       COALESCE((u.metadata ->> 'create_time')::timestamp, CURRENT_TIMESTAMP)
FROM {ACTIVE_EPISODIC_EMBEDDING_TABLE:adi_character_episodic_memory_embedding_bge_512} u
         LEFT JOIN adi_character ch ON ch.id = (u.metadata ->> 'conv_id')::bigint
WHERE (u.metadata ->> 'conv_id') IS NOT NULL
  AND u."text" IS NOT NULL
  AND u."text" <> ''
ON CONFLICT (uuid) DO NOTHING;

-- Step 3-2: backfill semantic rows from the semantic vector store.
-- create_time takes the migration moment: vector metadata never carried a creation time, so the original is unrecoverable.
INSERT INTO adi_character_memory_semantic
    (uuid, character_id, content, user_id, hit_count, create_time)
SELECT replace(vec.embedding_id::text, '-', ''),
       (vec.metadata ->> 'conv_id')::bigint,
       vec."text",
       COALESCE(ch.user_id, -1),
       COALESCE(vec.hit_count, 0),
       CURRENT_TIMESTAMP
FROM {ACTIVE_SEMANTIC_EMBEDDING_TABLE:adi_character_memory_embedding_bge_512} vec
         LEFT JOIN adi_character ch ON ch.id = (vec.metadata ->> 'conv_id')::bigint
WHERE vec.metadata ->> 'conv_id' IS NOT NULL
  AND vec."text" IS NOT NULL
  AND vec."text" <> ''
ON CONFLICT (uuid) DO NOTHING;

-- Step 3-3: post-backfill reconciliation. In a stopped-service window the counts must satisfy:
--   rel_episodic_alive = vec_episodic
--   rel_semantic_alive = vec_semantic
-- The equalities hold even when the double-write jar ran before this backfill (ON CONFLICT skips its rows; its deletions miss both counts).
-- They also assume every vector row passes the 3-1/3-2 guards (conv_id present, text non-blank); a row dropped by those guards shows up as rel < vec.
-- Sustained small drift only means writes happened between the backfill and this query.
SELECT (SELECT count(*) FROM adi_character_memory_episodic WHERE NOT is_deleted)  AS rel_episodic_alive,
       (SELECT count(*) FROM {ACTIVE_EPISODIC_EMBEDDING_TABLE:adi_character_episodic_memory_embedding_bge_512})  AS vec_episodic,
       (SELECT count(*) FROM adi_character_memory_semantic WHERE NOT is_deleted)  AS rel_semantic_alive,
       (SELECT count(*) FROM {ACTIVE_SEMANTIC_EMBEDDING_TABLE:adi_character_memory_embedding_bge_512})  AS vec_semantic;


-- ============================================================
-- Section 4: data migration when neo4j is used as the memory vector store / 使用 neo4j 做为记忆向量库的情况下的数据迁移
--
-- Replaces Section 3 when adi.vector-database = neo4j: memory vectors live in neo4j, plain SQL cannot query them, export them with Cypher into a staging table first; follow the steps below to run the migration.
-- Node labels (base names match the pgvector tables; the semantic label keeps the legacy "conversation" name; suffix per the Section 3 rule):
--   adi_conversation_memory_embedding{suffix}          semantic memory
--   adi_character_episodic_memory_embedding{suffix}    episodic memory
-- Segment metadata is stored as flat node properties under the same keys as the pgvector metadata JSON; nodes carry no hit_count (use 0).
--
-- ------------------------------------------------------------
--
-- adi.vector-database = neo4j 时替代 Section 3：记忆向量存储在 neo4j 中，普通 SQL 无法查询，需用 Cypher 导出到临时表；按以下步骤执行迁移。
-- 节点 label（基础名与 pgvector 表名一致；语义记忆 label 保留旧名 "conversation"；后缀规则同 Section 3）：
--   adi_conversation_memory_embedding{后缀}          语义记忆
--   adi_character_episodic_memory_embedding{后缀}    情景记忆
-- 段落 metadata 以平铺属性存放，键名与 pgvector 的 metadata JSON 相同；节点没有 hit_count（填 0）。
-- ============================================================

-- Step 4-1: create the staging table.
CREATE TABLE IF NOT EXISTS tmp_neo4j_memory_export
(
    embedding_id text,
    text         text,
    conv_id      bigint,
    memory_type  varchar(20),
    event_type   varchar(20),
    importance   int,
    conv_msg_id  bigint,
    create_time  timestamp
);

-- Step 4-2: export both labels (Neo4j Browser "Export CSV" or apoc.export.csv.query); labels shown for the default install.
-- Episodic store:
--       MATCH (n:`adi_character_episodic_memory_embedding_bge_512`)
--       RETURN n.id AS embedding_id, n.text AS text, n.conv_id AS conv_id,
--              'episodic' AS memory_type, n.event_type AS event_type, n.importance AS importance,
--              n.conv_msg_id AS conv_msg_id, n.create_time AS create_time
-- Semantic label:
--       MATCH (n:`adi_conversation_memory_embedding_bge_512`)
--       RETURN n.id AS embedding_id, n.text AS text, n.conv_id AS conv_id,
--              n.memory_type AS memory_type, n.event_type AS event_type, n.importance AS importance,
--              n.conv_msg_id AS conv_msg_id, n.create_time AS create_time

-- Step 4-3: load both CSVs into the staging table (column order matches the RETURN order above).
-- \copy tmp_neo4j_memory_export FROM 'episodic.csv' WITH (FORMAT csv, HEADER true)
-- \copy tmp_neo4j_memory_export FROM 'semantic.csv' WITH (FORMAT csv, HEADER true)

-- Step 4-4: backfill episodic rows (episodic.csv rows only; the memory_type filter separates the two CSVs inside the staging table), mirroring 3-1.
INSERT INTO adi_character_memory_episodic
    (uuid, character_id, content, event_type, importance, source_msg_id, user_id, hit_count, create_time)
SELECT replace(u.embedding_id, '-', ''),
       u.conv_id,
       u."text",
       COALESCE(u.event_type, ''),
       COALESCE(u.importance, 3),
       COALESCE(u.conv_msg_id, -1),
       COALESCE(ch.user_id, -1),
       0,
       COALESCE(u.create_time, CURRENT_TIMESTAMP)
FROM tmp_neo4j_memory_export u
         LEFT JOIN adi_character ch ON ch.id = u.conv_id
WHERE u.conv_id IS NOT NULL
  AND u.memory_type = 'episodic'
  AND u."text" IS NOT NULL
  AND u."text" <> ''
ON CONFLICT (uuid) DO NOTHING;

-- Step 4-5: backfill semantic rows (semantic.csv rows), mirroring 3-2.
-- create_time takes the migration moment: the semantic metadata never carried a creation time.
INSERT INTO adi_character_memory_semantic
    (uuid, character_id, content, user_id, hit_count, create_time)
SELECT replace(vec.embedding_id, '-', ''),
       vec.conv_id,
       vec."text",
       COALESCE(ch.user_id, -1),
       0,
       CURRENT_TIMESTAMP
FROM tmp_neo4j_memory_export vec
         LEFT JOIN adi_character ch ON ch.id = vec.conv_id
WHERE vec.conv_id IS NOT NULL
  AND COALESCE(vec.memory_type, 'semantic') <> 'episodic'
  AND vec."text" IS NOT NULL
  AND vec."text" <> ''
ON CONFLICT (uuid) DO NOTHING;

-- Step 4-6: post-backfill reconciliation. In a stopped-service window the counts must satisfy:
--   rel_episodic_alive = exported_episodic
--   rel_semantic_alive = exported_semantic
-- The equalities hold even when the double-write jar ran before this backfill (same reasoning as 3-3).
SELECT (SELECT count(*) FROM adi_character_memory_episodic WHERE NOT is_deleted)                             AS rel_episodic_alive,
       (SELECT count(*) FROM tmp_neo4j_memory_export WHERE memory_type = 'episodic')                         AS exported_episodic,
       (SELECT count(*) FROM adi_character_memory_semantic WHERE NOT is_deleted)                             AS rel_semantic_alive,
       (SELECT count(*) FROM tmp_neo4j_memory_export WHERE COALESCE(memory_type, 'semantic') <> 'episodic')  AS exported_semantic;
