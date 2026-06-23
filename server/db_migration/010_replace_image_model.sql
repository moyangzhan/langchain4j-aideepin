-- ============================================================
-- wan2.7-image: replace text-to-image model (new image-generation API paradigm,
-- supersedes wanx2.1-t2i-turbo which only supports the legacy image-synthesis endpoint).
-- ============================================================
-- Soft-delete the superseded model (keep the row; AiModel queries filter is_deleted = false).
UPDATE adi_ai_model SET is_deleted = true
WHERE name = 'wanx2.1-t2i-turbo' AND platform = 'dashscope' AND is_deleted = false;

INSERT INTO adi_ai_model (name, title, type, platform, properties, is_enable)
VALUES ('wan2.7-image', 'Qwen-Image', 'image', 'dashscope',
        '{"max_images": 4, "sizes": [{"value":"1K","label":"1K · 1024×1024"},{"value":"2K","label":"2K · 2048×2048"}]}',
        false);
