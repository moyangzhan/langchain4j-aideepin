-- ============================================================
-- Unified image generation (t2i + i2i).
-- wanx-background-generation-v2 was taken offline by Bailian notice 118434
-- (effective 2026-10-10) and is soft-deleted here; image editing is re-provided
-- as reference-image input on wan2.7-image / qwen-image-3.0 / gpt-image-2.
-- Capability declaration reuses the existing input_types column
-- (for type='image' models: 'text' = text-to-image, 'image' = image-to-image;
--  column default 'text' means text-to-image only — same semantics the retired
--  background model already used with 'text,image').
-- ============================================================

-- 1. Soft-delete the offline background model (keep the row; AiModel queries filter is_deleted = false).
--    Its historical adi_draw rows (interacting_method = 4) are intentionally kept; the display side renders them for compatibility.
UPDATE adi_ai_model SET is_deleted = true
WHERE name = 'wanx-background-generation-v2' AND platform = 'dashscope';

-- 2. wan2.7-image: retitle (was misleadingly 'Qwen-Image'), declare i2i input, ref-image limit, seed support, bilingual remark.
UPDATE adi_ai_model SET title = 'Wan2.7-Image',
    input_types = 'text,image',
    properties = '{"max_images": 4, "max_ref_images": 9, "is_support_seed": true, "qualities": [],
                   "sizes": [{"value":"1K","label":"1K · 1024×1024"},{"value":"2K","label":"2K · 2048×2048"}]}'::jsonb,
    remark = '通义万相旗舰图像模型，侧重全链路图像编辑：支持文生图、图生组图、图像编辑与多图参考生成（最多 9 张参考图）、交互式编辑；文生图最高 4K，编辑场景最高 2K。适合通用图片生成与多图参考编辑场景。/ Wan flagship image model focused on full-chain editing: text-to-image, storyboard generation, editing with up to 9 reference images, and interactive editing; up to 4K for T2I and 2K for editing. Best suited for general-purpose generation and multi-reference editing.'
WHERE name = 'wan2.7-image' AND platform = 'dashscope';

-- 3. qwen-image-3.0 (disabled by default; enable per deployment in the admin console).
INSERT INTO adi_ai_model (name, title, type, platform, input_types, properties, remark, is_enable)
VALUES ('qwen-image-3.0', 'Qwen-Image-3.0', 'image', 'dashscope', 'text,image',
        '{"max_images": 4, "max_ref_images": 3, "is_support_seed": true, "qualities": [],
          "sizes": [{"value":"1024*1024","label":"1024×1024"},{"value":"1280*960","label":"1280×960"},{"value":"1664*928","label":"1664×928"},{"value":"928*1664","label":"928×1664"}]}',
        '千问图像生成与编辑模型，侧重设计生产力：复杂版面、图中图、密集信息排版、高保真文字渲染（10px 小字、12 国语言），适合海报、网页、UI 等设计场景；图生图最多 3 张参考图。/ Qwen image generation and editing model oriented to design productivity: complex layouts, image-in-image, dense typography, and high-fidelity text rendering (10px text, 12 languages); ideal for posters, web pages, and UI design. Supports up to 3 reference images for editing.',
        false);

-- 4. gpt-image-2: declare i2i input (via the /images/edits endpoint); properties fully restated
--    (max_images was introduced by migration 008, kept at 10).
UPDATE adi_ai_model SET input_types = 'text,image',
    properties = '{"max_images": 10, "max_ref_images": 4, "is_support_seed": false, "qualities": ["auto","low","medium","high"],
                   "sizes": [{"value":"auto","label":"Auto"},{"value":"1024x1024","label":"1024×1024"},{"value":"1024x1536","label":"1024×1536"},{"value":"1536x1024","label":"1536×1024"}]}'::jsonb
WHERE name = 'gpt-image-2' AND platform = 'openai';

-- 5. Kwai-Kolors/Kolors: dual capability (t2i + i2i via the single-image `image` param of
--    /v1/images/generations); properties fully restated (max_images from 008; the legacy
--    image_sizes key disappears with the overwrite).
UPDATE adi_ai_model SET input_types = 'text,image',
    properties = '{"max_images": 4, "max_ref_images": 1, "is_support_seed": true, "qualities": [],
                   "sizes": [{"value":"1024x1024","label":"1024×1024"},{"value":"960x1280","label":"960×1280"},{"value":"768x1024","label":"768×1024"},{"value":"720x1440","label":"720×1440"},{"value":"720x1280","label":"720×1280"}]}'::jsonb
WHERE name = 'Kwai-Kolors/Kolors' AND platform = 'siliconflow';
