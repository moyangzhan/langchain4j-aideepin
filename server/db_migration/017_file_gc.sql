-- File GC preparation: rebuild adi_file.ref_count from live business references and seed the
-- file_gc_enabled switch (default false).
--
-- OPTIONAL migration. Skipping this file is safe: data created after the accompanying release
-- carries a correct ref_count on its own, and a missing file_gc_enabled row simply keeps the
-- automatic GC job off. But be aware that without this migration:
--   * historical files that are actually unreferenced keep a stale non-zero ref_count, never
--     enter the GC candidate set and may linger forever;
--   * the automatic GC cannot be enabled from the admin config page (the admin manual-cleanup
--     menu works either way).
--
-- Idempotent: reruns are no-ops — both UPDATEs carry a difference guard, the INSERT is NOT EXISTS,
-- the index is IF NOT EXISTS. If the index creation fails on duplicated uuids (only possible from
-- manual data edits), the statements before it are already committed and are themselves idempotent:
-- deduplicate (preview with
--   SELECT uuid, COUNT(*) FROM adi_file GROUP BY uuid HAVING COUNT(*) > 1;)
-- and rerun the file.
--
-- Dry run: before executing the UPDATEs, copy the whole "WITH refs AS ( ... )" block from either
-- UPDATE below in front of this SELECT to preview every row the rebuild would touch:
--   SELECT af.uuid, af.ref_count AS stored_ref_count, COALESCE(s.total, 0) AS recounted
--   FROM adi_file af
--   LEFT JOIN (SELECT file_uuid, SUM(cnt) AS total FROM refs GROUP BY file_uuid) s
--     ON af.uuid = s.file_uuid
--   WHERE af.ref_count <> COALESCE(s.total, 0);

-- ---------------------------------------------------------------------------
-- UPDATE 1/2: files with live references get their recount written back.
-- ---------------------------------------------------------------------------

WITH refs AS (
    -- draw: dynamic_params.ref_image_uuids (image-to-image reference images)
    SELECT je.value AS file_uuid, COUNT(*) AS cnt
    FROM adi_draw d,
         jsonb_array_elements_text(
             CASE WHEN jsonb_typeof(d.dynamic_params -> 'ref_image_uuids') = 'array'
                  THEN d.dynamic_params -> 'ref_image_uuids' ELSE '[]'::jsonb END) je
    WHERE d.is_deleted = false
    GROUP BY je.value
    UNION ALL
    -- draw: generated_images (comma-separated file uuids)
    SELECT u.uuid AS file_uuid, COUNT(*) AS cnt
    FROM adi_draw d, unnest(string_to_array(d.generated_images, ',')) u(uuid)
    WHERE d.is_deleted = false AND d.generated_images <> ''
    GROUP BY u.uuid
    UNION ALL
    -- draw: original_image (legacy OpenAI inpainting, legacy rows only)
    SELECT d.original_image AS file_uuid, COUNT(*) AS cnt
    FROM adi_draw d
    WHERE d.is_deleted = false AND d.original_image <> ''
    GROUP BY d.original_image
    UNION ALL
    -- draw: mask_image (legacy OpenAI inpainting, legacy rows only)
    SELECT d.mask_image AS file_uuid, COUNT(*) AS cnt
    FROM adi_draw d
    WHERE d.is_deleted = false AND d.mask_image <> ''
    GROUP BY d.mask_image
    UNION ALL
    -- character_message: attachments (comma-separated file uuids)
    SELECT u.uuid AS file_uuid, COUNT(*) AS cnt
    FROM adi_character_message m, unnest(string_to_array(m.attachments, ',')) u(uuid)
    WHERE m.is_deleted = false AND m.attachments <> ''
    GROUP BY u.uuid
    UNION ALL
    -- character_message: audio_uuid
    SELECT m.audio_uuid AS file_uuid, COUNT(*) AS cnt
    FROM adi_character_message m
    WHERE m.is_deleted = false AND m.audio_uuid <> ''
    GROUP BY m.audio_uuid
    UNION ALL
    -- workflow_runtime.input: type=4 file parameters; URL-shaped values (local /file/{uuid}.{ext}
    -- links, OSS object URLs — draw-node products) normalize back to the uuid via last path
    -- segment without extension, plain uuids pass through
    SELECT
        CASE WHEN fe.value LIKE 'http%' OR fe.value LIKE '/file/%'
             THEN regexp_replace(regexp_replace(fe.value, '^.*/', ''), '\.[^.]+$', '')
             ELSE fe.value END AS file_uuid,
        COUNT(*) AS cnt
    FROM adi_workflow_runtime wr,
         jsonb_each(wr.input) e,
         jsonb_array_elements_text(
             CASE WHEN e.value ->> 'type' = '4' AND jsonb_typeof(e.value -> 'value') = 'array'
                  THEN e.value -> 'value' ELSE '[]'::jsonb END) fe
    WHERE wr.is_deleted = false AND wr.input IS NOT NULL AND jsonb_typeof(wr.input) = 'object'
    GROUP BY fe.value
    UNION ALL
    SELECT
        CASE WHEN fe.value LIKE 'http%' OR fe.value LIKE '/file/%'
             THEN regexp_replace(regexp_replace(fe.value, '^.*/', ''), '\.[^.]+$', '')
             ELSE fe.value END AS file_uuid,
        COUNT(*) AS cnt
    FROM adi_workflow_runtime wr,
         jsonb_each(wr."output") e,
         jsonb_array_elements_text(
             CASE WHEN e.value ->> 'type' = '4' AND jsonb_typeof(e.value -> 'value') = 'array'
                  THEN e.value -> 'value' ELSE '[]'::jsonb END) fe
    WHERE wr.is_deleted = false AND wr."output" IS NOT NULL AND jsonb_typeof(wr."output") = 'object'
    GROUP BY fe.value
    UNION ALL
    -- workflow_runtime_node input/output: node rows belong to a runtime via workflow_runtime_id;
    -- a soft-deleted runtime drops its nodes' references (node rows themselves are never soft-deleted)
    SELECT
        CASE WHEN fe.value LIKE 'http%' OR fe.value LIKE '/file/%'
             THEN regexp_replace(regexp_replace(fe.value, '^.*/', ''), '\.[^.]+$', '')
             ELSE fe.value END AS file_uuid,
        COUNT(*) AS cnt
    FROM adi_workflow_runtime_node n,
         jsonb_each(n.input) e,
         jsonb_array_elements_text(
             CASE WHEN e.value ->> 'type' = '4' AND jsonb_typeof(e.value -> 'value') = 'array'
                  THEN e.value -> 'value' ELSE '[]'::jsonb END) fe
    WHERE n.is_deleted = false
      AND n.input IS NOT NULL AND jsonb_typeof(n.input) = 'object'
      AND EXISTS (SELECT 1 FROM adi_workflow_runtime wr
                  WHERE wr.id = n.workflow_runtime_id AND wr.is_deleted = false)
    GROUP BY fe.value
    UNION ALL
    SELECT
        CASE WHEN fe.value LIKE 'http%' OR fe.value LIKE '/file/%'
             THEN regexp_replace(regexp_replace(fe.value, '^.*/', ''), '\.[^.]+$', '')
             ELSE fe.value END AS file_uuid,
        COUNT(*) AS cnt
    FROM adi_workflow_runtime_node n,
         jsonb_each(n."output") e,
         jsonb_array_elements_text(
             CASE WHEN e.value ->> 'type' = '4' AND jsonb_typeof(e.value -> 'value') = 'array'
                  THEN e.value -> 'value' ELSE '[]'::jsonb END) fe
    WHERE n.is_deleted = false
      AND n."output" IS NOT NULL AND jsonb_typeof(n."output") = 'object'
      AND EXISTS (SELECT 1 FROM adi_workflow_runtime wr
                  WHERE wr.id = n.workflow_runtime_id AND wr.is_deleted = false)
    GROUP BY fe.value
    UNION ALL
    -- document: source_file_id (Long id, join adi_file)
    SELECT f.uuid AS file_uuid, COUNT(*) AS cnt
    FROM adi_document kd
    JOIN adi_file f ON f.id = kd.source_file_id
    WHERE kd.is_deleted = false AND kd.source_file_id IS NOT NULL
    GROUP BY f.uuid
    UNION ALL
    -- knowledge_base_message.source_file_ids: dead column (zero write sites in the current code,
    -- storage format unverified). Defensive counting matches BOTH candidate formats — adi_file id
    -- and adi_file uuid — so either shape yields references (over-counting is the safe direction);
    -- if a production sample shows the column entirely empty, this whole block can be deleted.
    SELECT f.uuid AS file_uuid, COUNT(*) AS cnt
    FROM adi_knowledge_base_message kbm
    JOIN adi_file f
      ON (f.id::text = ANY(string_to_array(kbm.source_file_ids, ','))
          OR f.uuid = ANY(string_to_array(kbm.source_file_ids, ',')))
    WHERE kbm.is_deleted = false AND kbm.source_file_ids <> ''
    GROUP BY f.uuid
)
UPDATE adi_file af SET ref_count = COALESCE(s.total, 0)
FROM (SELECT file_uuid, SUM(cnt) AS total FROM refs GROUP BY file_uuid) s
WHERE af.uuid = s.file_uuid AND af.ref_count <> s.total;

-- ---------------------------------------------------------------------------
-- UPDATE 2/2: files referenced by nothing reset to zero. A CTE only applies to the statement
-- right after it, so this statement repeats the WITH refs block verbatim.
-- ---------------------------------------------------------------------------

WITH refs AS (
    SELECT je.value AS file_uuid, COUNT(*) AS cnt
    FROM adi_draw d,
         jsonb_array_elements_text(
             CASE WHEN jsonb_typeof(d.dynamic_params -> 'ref_image_uuids') = 'array'
                  THEN d.dynamic_params -> 'ref_image_uuids' ELSE '[]'::jsonb END) je
    WHERE d.is_deleted = false
    GROUP BY je.value
    UNION ALL
    SELECT u.uuid AS file_uuid, COUNT(*) AS cnt
    FROM adi_draw d, unnest(string_to_array(d.generated_images, ',')) u(uuid)
    WHERE d.is_deleted = false AND d.generated_images <> ''
    GROUP BY u.uuid
    UNION ALL
    SELECT d.original_image AS file_uuid, COUNT(*) AS cnt
    FROM adi_draw d
    WHERE d.is_deleted = false AND d.original_image <> ''
    GROUP BY d.original_image
    UNION ALL
    SELECT d.mask_image AS file_uuid, COUNT(*) AS cnt
    FROM adi_draw d
    WHERE d.is_deleted = false AND d.mask_image <> ''
    GROUP BY d.mask_image
    UNION ALL
    SELECT u.uuid AS file_uuid, COUNT(*) AS cnt
    FROM adi_character_message m, unnest(string_to_array(m.attachments, ',')) u(uuid)
    WHERE m.is_deleted = false AND m.attachments <> ''
    GROUP BY u.uuid
    UNION ALL
    SELECT m.audio_uuid AS file_uuid, COUNT(*) AS cnt
    FROM adi_character_message m
    WHERE m.is_deleted = false AND m.audio_uuid <> ''
    GROUP BY m.audio_uuid
    UNION ALL
    SELECT
        CASE WHEN fe.value LIKE 'http%' OR fe.value LIKE '/file/%'
             THEN regexp_replace(regexp_replace(fe.value, '^.*/', ''), '\.[^.]+$', '')
             ELSE fe.value END AS file_uuid,
        COUNT(*) AS cnt
    FROM adi_workflow_runtime wr,
         jsonb_each(wr.input) e,
         jsonb_array_elements_text(
             CASE WHEN e.value ->> 'type' = '4' AND jsonb_typeof(e.value -> 'value') = 'array'
                  THEN e.value -> 'value' ELSE '[]'::jsonb END) fe
    WHERE wr.is_deleted = false AND wr.input IS NOT NULL AND jsonb_typeof(wr.input) = 'object'
    GROUP BY fe.value
    UNION ALL
    SELECT
        CASE WHEN fe.value LIKE 'http%' OR fe.value LIKE '/file/%'
             THEN regexp_replace(regexp_replace(fe.value, '^.*/', ''), '\.[^.]+$', '')
             ELSE fe.value END AS file_uuid,
        COUNT(*) AS cnt
    FROM adi_workflow_runtime wr,
         jsonb_each(wr."output") e,
         jsonb_array_elements_text(
             CASE WHEN e.value ->> 'type' = '4' AND jsonb_typeof(e.value -> 'value') = 'array'
                  THEN e.value -> 'value' ELSE '[]'::jsonb END) fe
    WHERE wr.is_deleted = false AND wr."output" IS NOT NULL AND jsonb_typeof(wr."output") = 'object'
    GROUP BY fe.value
    UNION ALL
    SELECT
        CASE WHEN fe.value LIKE 'http%' OR fe.value LIKE '/file/%'
             THEN regexp_replace(regexp_replace(fe.value, '^.*/', ''), '\.[^.]+$', '')
             ELSE fe.value END AS file_uuid,
        COUNT(*) AS cnt
    FROM adi_workflow_runtime_node n,
         jsonb_each(n.input) e,
         jsonb_array_elements_text(
             CASE WHEN e.value ->> 'type' = '4' AND jsonb_typeof(e.value -> 'value') = 'array'
                  THEN e.value -> 'value' ELSE '[]'::jsonb END) fe
    WHERE n.is_deleted = false
      AND n.input IS NOT NULL AND jsonb_typeof(n.input) = 'object'
      AND EXISTS (SELECT 1 FROM adi_workflow_runtime wr
                  WHERE wr.id = n.workflow_runtime_id AND wr.is_deleted = false)
    GROUP BY fe.value
    UNION ALL
    SELECT
        CASE WHEN fe.value LIKE 'http%' OR fe.value LIKE '/file/%'
             THEN regexp_replace(regexp_replace(fe.value, '^.*/', ''), '\.[^.]+$', '')
             ELSE fe.value END AS file_uuid,
        COUNT(*) AS cnt
    FROM adi_workflow_runtime_node n,
         jsonb_each(n."output") e,
         jsonb_array_elements_text(
             CASE WHEN e.value ->> 'type' = '4' AND jsonb_typeof(e.value -> 'value') = 'array'
                  THEN e.value -> 'value' ELSE '[]'::jsonb END) fe
    WHERE n.is_deleted = false
      AND n."output" IS NOT NULL AND jsonb_typeof(n."output") = 'object'
      AND EXISTS (SELECT 1 FROM adi_workflow_runtime wr
                  WHERE wr.id = n.workflow_runtime_id AND wr.is_deleted = false)
    GROUP BY fe.value
    UNION ALL
    SELECT f.uuid AS file_uuid, COUNT(*) AS cnt
    FROM adi_document kd
    JOIN adi_file f ON f.id = kd.source_file_id
    WHERE kd.is_deleted = false AND kd.source_file_id IS NOT NULL
    GROUP BY f.uuid
    UNION ALL
    SELECT f.uuid AS file_uuid, COUNT(*) AS cnt
    FROM adi_knowledge_base_message kbm
    JOIN adi_file f
      ON (f.id::text = ANY(string_to_array(kbm.source_file_ids, ','))
          OR f.uuid = ANY(string_to_array(kbm.source_file_ids, ',')))
    WHERE kbm.is_deleted = false AND kbm.source_file_ids <> ''
    GROUP BY f.uuid
)
UPDATE adi_file SET ref_count = 0
WHERE ref_count <> 0 AND uuid NOT IN (SELECT file_uuid FROM refs WHERE file_uuid IS NOT NULL);

-- ---------------------------------------------------------------------------
-- GC switch row: single boolean gate for the whole automatic GC job (both stages).
-- Default false — the first run faces the full historical backlog, a destructive action an
-- admin enables explicitly. Effective on the next job round after the value is saved.
-- ---------------------------------------------------------------------------

INSERT INTO adi_sys_config (name, value)
SELECT 'file_gc_enabled', 'false'
WHERE NOT EXISTS (SELECT 1 FROM adi_sys_config WHERE name = 'file_gc_enabled');

-- ---------------------------------------------------------------------------
-- uuid lookup index: reference attach/detach, the recount IN-lists and the existing by-uuid
-- reads all locate adi_file rows by uuid; the table has no index on it so every such query
-- degrades to a full scan. uuid is globally unique (a soft-deleted row keeps its uuid, a
-- re-upload of the same content gets a new one). Idempotent.
-- ---------------------------------------------------------------------------

create unique index if not exists uk_file_uuid on adi_file (uuid);
