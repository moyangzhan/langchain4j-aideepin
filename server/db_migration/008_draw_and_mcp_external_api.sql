-- ============================================================
-- Migration: Add user-level External API support for Draw & MCP
-- Description:
--   1. Create adi_user_ext_api_key table for user-level API keys
--   2. Update generate_number check constraint in adi_draw
--   3. Add max_images property to image models
-- ============================================================

-- 1. Create user ext api key table
CREATE TABLE adi_user_ext_api_key
(
    id            bigserial primary key,
    user_id       bigint        default 0                 not null,
    resource_type varchar(20)   default ''                not null,
    api_key       varchar(256)  default ''                not null,
    create_time   timestamp     default CURRENT_TIMESTAMP not null,
    update_time   timestamp     default CURRENT_TIMESTAMP not null,
    is_deleted    boolean       default false             not null
);

CREATE UNIQUE INDEX udx_user_resource_type
    ON adi_user_ext_api_key(user_id, resource_type)
    WHERE is_deleted = false;

COMMENT ON TABLE adi_user_ext_api_key IS 'User-level External API Key';
COMMENT ON COLUMN adi_user_ext_api_key.user_id IS 'User ID';
COMMENT ON COLUMN adi_user_ext_api_key.resource_type IS 'Resource type: draw, mcp';
COMMENT ON COLUMN adi_user_ext_api_key.api_key IS 'Encrypted External API Key';

-- 2. Update generate_number check constraint (remove hardcoded max 10)
ALTER TABLE adi_draw DROP CONSTRAINT IF EXISTS adi_draw_generate_number_check;
ALTER TABLE adi_draw ADD CONSTRAINT adi_draw_generate_number_check CHECK ((generate_number >= 1));

-- 3. Add max_images to image models
-- OpenAI gpt-image-2
UPDATE adi_ai_model
SET properties = jsonb_set(
    COALESCE(properties, '{}')::jsonb,
    '{max_images}',
    '10'::jsonb
)
WHERE name = 'gpt-image-2' AND platform = 'openai';

-- DashScope wanx models
UPDATE adi_ai_model
SET properties = jsonb_set(
    COALESCE(properties, '{}')::jsonb,
    '{max_images}',
    '4'::jsonb
)
WHERE name IN ('wanx2.1-t2i-turbo', 'wan2.5-t2i-preview', 'wan2.2-t2i-flash', 'wan2.2-t2i-plus')
  AND platform = 'dashscope';

-- SiliconFlow Kolors
UPDATE adi_ai_model
SET properties = jsonb_set(
    COALESCE(properties, '{}')::jsonb,
    '{max_images}',
    '4'::jsonb
)
WHERE name = 'Kwai-Kolors/Kolors' AND platform = 'siliconflow';
