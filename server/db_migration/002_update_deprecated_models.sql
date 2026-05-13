-- noinspection SqlNoDataSourceInspectionForFile

-- DALL-E 2 和 DALL-E 3 API 将于 2026-05-12 永久关闭
-- 替换为 OpenAI 当前主推的 gpt-image-2

-- 禁用旧的 DALL-E 模型（防止已有数据外键引用问题，保留记录）
UPDATE adi_ai_model SET is_enable = false, remark = 'DALL-E API deprecated, will be removed on 2026-05-12' WHERE name IN ('dall-e-2', 'dall-e-3');

-- 新增 gpt-image-2 模型（如果不存在）
INSERT INTO adi_ai_model (name, title, type, platform, is_enable)
SELECT 'gpt-image-2', 'GPT-Image-2', 'image', 'openai', false
WHERE NOT EXISTS (SELECT 1 FROM adi_ai_model WHERE name = 'gpt-image-2');

-- DeepSeek 统一为 deepseek-v4-flash（通过 thinking 参数控制是否深度思考）
-- 禁用旧的 deepseek-chat 和 deepseek-reasoner
UPDATE adi_ai_model SET is_enable = false, remark = 'Replaced by deepseek-v4-flash' WHERE name IN ('deepseek-chat', 'deepseek-reasoner');

-- 新增 deepseek-v4-flash 模型（如果不存在）
INSERT INTO adi_ai_model (name, title, type, platform, context_window, max_input_tokens, max_output_tokens,
                          response_format_types, is_reasoner, is_thinking_closable, is_enable)
SELECT 'deepseek-v4-flash', 'DeepSeek-V4-Flash', 'text', 'deepseek', 1000000, 980000, 384000, 'text,json_object', true, true, false
WHERE NOT EXISTS (SELECT 1 FROM adi_ai_model WHERE name = 'deepseek-v4-flash');

-- 工作流组件：Dalle3 重命名为 OpenAiImage
UPDATE adi_workflow_component SET name = 'OpenAiImage', title = 'OpenAI 画图', remark = '调用OpenAI图片模型生成图片' WHERE name = 'Dalle3';

-- 千帆平台停用（ERNIE-Speed-128K 已于 2026-01-27 下架，deepseek-v4-flash 全面覆盖其能力）
UPDATE adi_ai_model SET is_enable = false, remark = 'Qianfan deprecated, ERNIE-Speed-128K delisted on 2026-01-27' WHERE platform = 'qianfan';
UPDATE adi_model_platform SET is_enable = false WHERE name = 'qianfan';

-- 工作流节点中的 ERNIE-Speed-128K 替换为 deepseek-v4-flash
UPDATE adi_workflow_node SET node_config = REPLACE(node_config::text, '"model_name": "ERNIE-Speed-128K"', '"model_name": "deepseek-v4-flash"')::jsonb WHERE node_config::text LIKE '%"model_name": "ERNIE-Speed-128K"%';
