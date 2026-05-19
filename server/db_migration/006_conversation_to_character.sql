-- ============================================================
-- Migration: Rename adi_conversation* tables/columns to adi_character*
-- Description: The system uses "Character" (persistent persona with
--   long-term memory) instead of "Conversation" (one-off session).
--   This migration aligns the database schema with the code-level rename.
-- ============================================================

-- 1. adi_conversation → adi_character
ALTER TABLE adi_conversation RENAME TO adi_character;
-- Update trigger
DROP TRIGGER trigger_conv_update_time ON adi_character;
CREATE TRIGGER trigger_character_update_time
    BEFORE UPDATE
    ON adi_character
    FOR EACH ROW
EXECUTE PROCEDURE update_modified_column();

-- 2. adi_conversation_preset → adi_character_preset
ALTER TABLE adi_conversation_preset RENAME TO adi_character_preset;
DROP TRIGGER trigger_conversation_preset ON adi_character_preset;
CREATE TRIGGER trigger_character_preset
    BEFORE UPDATE
    ON adi_character_preset
    FOR EACH ROW
EXECUTE PROCEDURE update_modified_column();

-- 3. adi_conversation_preset_rel → adi_character_preset_rel
ALTER TABLE adi_conversation_preset_rel RENAME TO adi_character_preset_rel;
ALTER TABLE adi_character_preset_rel RENAME COLUMN preset_conv_id TO preset_character_id;
ALTER TABLE adi_character_preset_rel RENAME COLUMN user_conv_id TO user_character_id;
DROP TRIGGER trigger_conversation_preset_rel ON adi_character_preset_rel;
CREATE TRIGGER trigger_character_preset_rel
    BEFORE UPDATE
    ON adi_character_preset_rel
    FOR EACH ROW
EXECUTE PROCEDURE update_modified_column();

-- 4. adi_conversation_message → adi_character_message
ALTER TABLE adi_conversation_message RENAME TO adi_character_message;
ALTER TABLE adi_character_message RENAME COLUMN conversation_id TO character_id;
ALTER TABLE adi_character_message RENAME COLUMN conversation_uuid TO character_uuid;
DROP TRIGGER trigger_conv_message_update_time ON adi_character_message;
CREATE TRIGGER trigger_character_message_update_time
    BEFORE UPDATE
    ON adi_character_message
    FOR EACH ROW
EXECUTE PROCEDURE update_modified_column();

-- 5. adi_conversation_message_ref_embedding → adi_character_message_ref_embedding
ALTER TABLE adi_conversation_message_ref_embedding RENAME TO adi_character_message_ref_embedding;

-- 6. adi_conversation_message_ref_graph → adi_character_message_ref_graph
ALTER TABLE adi_conversation_message_ref_graph RENAME TO adi_character_message_ref_graph;

-- 7. adi_conversation_message_ref_memory_embedding → adi_character_message_ref_memory_embedding
ALTER TABLE adi_conversation_message_ref_memory_embedding RENAME TO adi_character_message_ref_memory_embedding;

-- 8. Dynamic PGVector embedding tables for character memory
--    These tables may have suffixes like _bge_384, _openai_1536, etc.
--    Rename base table and all suffixed variants.
DO $$
DECLARE
    tbl TEXT;
BEGIN
    -- Rename base table if it exists
    IF EXISTS (SELECT 1 FROM information_schema.tables WHERE table_name = 'adi_conversation_memory_embedding') THEN
        ALTER TABLE adi_conversation_memory_embedding RENAME TO adi_character_memory_embedding;
    END IF;

    -- Rename suffixed tables (e.g., adi_conversation_memory_embedding_bge_384)
    FOR tbl IN
        SELECT table_name FROM information_schema.tables
        WHERE table_name LIKE 'adi_conversation_memory_embedding_%'
          AND table_schema = 'public'
    LOOP
        EXECUTE format('ALTER TABLE %I RENAME TO %I',
            tbl,
            replace(tbl, 'adi_conversation_memory_embedding_', 'adi_character_memory_embedding_'));
    END LOOP;
END
$$;

-- ============================================================
-- Neo4j: No migration needed.
-- Neo4j uses node labels (not tables). The code intentionally keeps
-- the old label/index names (adi_conversation_memory_embedding / conv_memory)
-- to avoid complex label rename + vector index rebuild on existing data.
-- ============================================================

-- ============================================================
-- Important: metadata keys conv_id / conv_msg_id in vector stores
-- are NOT renamed to avoid breaking existing stored data.
-- ============================================================

-- 9. Update system config key
UPDATE adi_sys_config SET name = 'character_max_num' WHERE name = 'conversation_max_num';
