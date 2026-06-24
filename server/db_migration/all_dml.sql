-- ============================================================
-- Initial Data: seed data inserts
-- ============================================================

-- 管理员账号：catkeeper@aideepin.com  密码：123456
INSERT INTO adi_user (name, password, uuid, email, user_status, is_admin)
VALUES ('catkeeper', '$2a$10$z44gncmQk6xCBCeDx55gMe1Zc8uYtOKcoT4/HE2F92VcF7wP2iquG',
        replace(gen_random_uuid()::text, '-', ''), 'catkeeper@aideepin.com', 2, true);

-- 配置信息
-- https://api-docs.deepseek.com/zh-cn/
INSERT INTO adi_sys_config (name, value)
VALUES ('google_setting',
        '{"url":"https://www.googleapis.com/customsearch/v1","key":"","cx":""}');
INSERT INTO adi_sys_config (name, value)
VALUES ('request_text_rate_limit', '{"times":24,"minutes":3}');
INSERT INTO adi_sys_config (name, value)
VALUES ('request_image_rate_limit', '{"times":6,"minutes":3}');
INSERT INTO adi_sys_config (name, value)
VALUES ('character_max_num', '50');
INSERT INTO adi_sys_config (name, value)
VALUES ('quota_by_token_daily', '10000');
INSERT INTO adi_sys_config (name, value)
VALUES ('quota_by_token_monthly', '200000');
INSERT INTO adi_sys_config (name, value)
VALUES ('quota_by_request_daily', '150');
INSERT INTO adi_sys_config (name, value)
VALUES ('quota_by_request_monthly', '3000');
INSERT INTO adi_sys_config (name, value)
VALUES ('quota_by_image_daily', '30');
INSERT INTO adi_sys_config (name, value)
VALUES ('quota_by_image_monthly', '300');
INSERT INTO adi_sys_config (name, value)
VALUES ('quota_by_qa_ask_daily', '50');
INSERT INTO adi_sys_config (name, value)
VALUES ('quota_by_qa_item_monthly', '100');

-- 文件存储位置，默认存储到本地
-- store_location: 1表示存储到本地,2表示存储到阿里云oss
INSERT INTO adi_sys_config (name, value)
VALUES ('storage_location', '1');
-- endpoint: 如：oss-cn-hangzhou.aliyuncs.com
INSERT INTO adi_sys_config (name, value)
VALUES ('storage_location_ali_oss', '{"access_key_id":"","access_key_secret":"","endpoint":"","bucket_name":""}');

-- ASR settings
-- Supported platforms and models:
--   siliconflow  -> FunAudioLLM/SenseVoiceSmall
--   dashscope    -> paraformer-v2
--   openai       -> gpt-4o-mini-transcribe
-- Max file size: 10MB, max recording duration: 60s (TODO)
INSERT INTO adi_sys_config (name, value)
VALUES ('asr_setting',
        '{"model_name":"FunAudioLLM/SenseVoiceSmall","platform":"siliconflow","max_record_duration":60,"max_file_size":10485760}');

-- TTS settings
-- Supported platforms and models:
--   siliconflow  -> FunAudioLLM/CosyVoice2-0.5B
--   dashscope    -> cosyvoice-v3-flash
--   openai       -> gpt-4o-mini-tts
-- synthesizer_side: "client" uses browser TTS (free), "server" uses models (paid)
INSERT INTO adi_sys_config (name, value)
VALUES ('tts_setting', '{"synthesizer_side":"client","model_name":"","platform":""}');
-- Uncomment one of the following to enable server-side TTS with a specific model:
-- INSERT INTO adi_sys_config (name, value)
-- VALUES ('tts_setting', '{"synthesizer_side":"server","model_name":"cosyvoice-v3-flash","platform":"dashscope"}');
-- INSERT INTO adi_sys_config (name, value)
-- VALUES ('tts_setting', '{"synthesizer_side":"server","model_name":"gpt-4o-mini-tts","platform":"openai"}');
-- INSERT INTO adi_sys_config (name, value)
-- VALUES ('tts_setting', '{"synthesizer_side":"server","model_name":"FunAudioLLM/CosyVoice2-0.5B","platform":"siliconflow"}');

-- 模型平台
insert into adi_model_platform (name, title, base_url)
values ('openai', 'OpenAI', 'https://api.openai.com/v1');
insert into adi_model_platform (name, title, base_url)
values ('deepseek', 'DeepSeek', 'https://api.deepseek.com/v1');
insert into adi_model_platform (name, title, base_url)
values ('dashscope', 'DashScope', 'https://dashscope.aliyuncs.com/api/v1');
insert into adi_model_platform (name, title, base_url)
values ('siliconflow', 'SiliconFlow', 'https://api.siliconflow.cn/v1');
insert into adi_model_platform (name, title, base_url)
values ('ollama', 'ollama', 'http://localhost:11434');
-- SiliconFlow text models are compatible with OpenAI API; this row tests whether dynamically created platforms/models use OpenAiCompatibleLLMService correctly
insert into adi_model_platform (name, title, base_url, is_openai_api_compatible)
values ('openai-compatible-platform-test', 'OpenAI-Compatible Platform', 'https://api.siliconflow.cn/v1', true);

-- 大语言模型
-- https://api-docs.deepseek.com/zh-cn/quick_start/pricing
INSERT INTO adi_ai_model (name, title, type, platform, context_window, max_input_tokens, max_output_tokens,
                          response_format_types, is_reasoner,
                          is_thinking_closable, is_enable)
VALUES ('deepseek-v4-flash', 'DeepSeek-V4-Flash', 'text', 'deepseek', 1000000, 980000, 384000, 'text,json_object', true, true,
        false);

-- https://developers.openai.com/api/docs/models/gpt-5-mini (superseded by gpt-5.4-mini)
-- INSERT INTO adi_ai_model (name, title, type, platform, context_window, max_input_tokens, max_output_tokens,
--                           response_format_types, remark,
--                           is_enable)
-- VALUES ('gpt-5-mini', 'gpt-5-mini', 'text', 'openai', 400000, 272000, 128000, 'text,json_object',
--         'GPT-5 mini is a faster, more cost-efficient version of GPT-5. It''s great for well-defined tasks and precise prompts.',
--         false);

-- https://developers.openai.com/api/docs/models/gpt-5.4-mini
INSERT INTO adi_ai_model (name, title, type, platform, context_window, max_input_tokens, max_output_tokens,
                          response_format_types, remark,
                          is_enable)
VALUES ('gpt-5.4-mini', 'gpt-5.4-mini', 'text', 'openai', 400000, 272000, 128000, 'text,json_object',
        'GPT-5.4 mini brings the strengths of GPT-5.4 to a faster, more efficient model designed for high-volume workloads.',
        false);


INSERT INTO adi_ai_model (name, title, type, platform, properties, is_enable)
VALUES ('gpt-image-2', 'GPT-Image-2', 'image', 'openai', '{"max_images": 10}', false);
-- OpenAI 语音识别 (ASR)
INSERT INTO adi_ai_model (name, title, type, platform, input_types, is_enable)
VALUES ('gpt-4o-mini-transcribe', 'OpenAI-ASR', 'asr', 'openai', 'audio', false);
-- OpenAI 语音合成 (TTS)
-- gpt-4o-mini-tts supports all 13 voices
INSERT INTO adi_ai_model (name, title, type, platform, input_types, properties, is_enable)
VALUES ('gpt-4o-mini-tts', 'OpenAI-TTS', 'tts', 'openai', 'text', '{
  "voices": [
    {"name": "Alloy", "remark": "Alloy", "param_name": "alloy", "lang": "multilingual"},
    {"name": "Ash", "remark": "Ash", "param_name": "ash", "lang": "multilingual"},
    {"name": "Ballad", "remark": "Ballad", "param_name": "ballad", "lang": "multilingual"},
    {"name": "Coral", "remark": "Coral", "param_name": "coral", "lang": "multilingual"},
    {"name": "Echo", "remark": "Echo", "param_name": "echo", "lang": "multilingual"},
    {"name": "Fable", "remark": "Fable", "param_name": "fable", "lang": "multilingual"},
    {"name": "Nova", "remark": "Nova", "param_name": "nova", "lang": "multilingual"},
    {"name": "Onyx", "remark": "Onyx", "param_name": "onyx", "lang": "multilingual"},
    {"name": "Sage", "remark": "Sage", "param_name": "sage", "lang": "multilingual"},
    {"name": "Shimmer", "remark": "Shimmer", "param_name": "shimmer", "lang": "multilingual"},
    {"name": "Verse", "remark": "Verse", "param_name": "verse", "lang": "multilingual"},
    {"name": "Marin", "remark": "Marin", "param_name": "marin", "lang": "multilingual"},
    {"name": "Cedar", "remark": "Cedar", "param_name": "cedar", "lang": "multilingual"}
  ]
}', false);
INSERT INTO adi_ai_model (name, title, type, platform, max_input_tokens, properties, is_enable)
VALUES ('text-embedding-3-small', 'OpenAI-Embedding-Small', 'embedding', 'openai', 8191, '{
  "dimension": 1536
}', false);
INSERT INTO adi_ai_model (name, title, type, platform, max_input_tokens, properties, is_enable)
VALUES ('text-embedding-3-large', 'OpenAI-Embedding-Large', 'embedding', 'openai', 8191, '{
  "dimension": 3072
}', false);
-- https://help.aliyun.com/zh/dashscope/developer-reference/model-introduction?spm=a2c4g.11186623.0.i39
INSERT INTO adi_ai_model (name, title, type, platform, context_window, max_input_tokens, max_output_tokens,
                          response_format_types, is_support_web_search, is_reasoner,
                          is_thinking_closable, is_enable)
VALUES ('qwen-turbo', 'Qwen-Turbo', 'text', 'dashscope', 131072, 98304, 16384, 'text,json_object', true, true, true,
        false);
-- 图片识别
INSERT INTO adi_ai_model (name, title, type, platform, context_window, max_input_tokens, max_output_tokens, input_types,
                          is_enable)
VALUES ('qwen2-vl-7b-instruct', 'Qwen-Vision', 'vision', 'dashscope', 32768, 16384, 16384, 'text,image', false);
-- 通义万相-文生图 wan2.7-image
INSERT INTO adi_ai_model (name, title, type, platform, properties, is_enable)
VALUES ('wan2.7-image', 'Qwen-Image', 'image', 'dashscope', '{"max_images": 4, "sizes": [{"value":"1K","label":"1K · 1024×1024"},{"value":"2K","label":"2K · 2048×2048"}]}', false);
-- 通义万相-切换背景
INSERT INTO adi_ai_model (name, title, type, platform, input_types, is_enable)
VALUES ('wanx-background-generation-v2', 'Qwen-Background', 'image', 'dashscope', 'text,image', false);
-- 通义千问-向量（可选模型名：text-embedding-v1,text-embedding-v2,text-embedding-v3）
INSERT INTO adi_ai_model (name, title, type, platform, max_input_tokens, properties, is_enable)
VALUES ('text-embedding-v1', 'Qwen-Embedding-V1', 'embedding', 'dashscope', 2048, '{
  "dimension": 1536
}', false);
INSERT INTO adi_ai_model (name, title, type, platform, max_input_tokens, properties, is_enable)
VALUES ('text-embedding-v3', 'Qwen-Embedding-V3', 'embedding', 'dashscope', 8192, '{
  "dimension": 1024
}', false);
-- 语音识别
-- paraformer-v2 只支持公网可访问的音频文件，如需要传输本地音频文件，请激活使用下面硅基流动的SenseVoiceSmall
INSERT INTO adi_ai_model (name, title, type, platform, input_types, is_enable)
VALUES ('paraformer-v2', 'Qwen-ASR', 'asr', 'dashscope', 'audio', false);
-- 语音合成
INSERT INTO adi_ai_model (name, title, type, platform, input_types, properties, is_enable)
VALUES ('cosyvoice-v3-flash', 'Qwen-TTS', 'tts', 'dashscope', 'text', '{
  "voices": [
    {
      "name": "龙安洋",
      "remark": "阳光大男孩",
      "param_name": "longanyang",
      "lang": "中、英"
    },
    {
      "name": "龙安欢",
      "remark": "欢脱元气女",
      "param_name": "longanhuan",
      "lang": "中、英"
    },
    {
      "name": "龙呼呼",
      "remark": "天真烂漫女童",
      "param_name": "longhuhu_v3",
      "lang": "中、英"
    },
    {
      "name": "龙泡泡",
      "remark": "飞天泡泡音",
      "param_name": "longpaopao_v3",
      "lang": "中、英"
    },
    {
      "name": "龙杰力豆",
      "remark": "阳光顽皮男",
      "param_name": "longjielidou_v3",
      "lang": "中、英"
    },
    {
      "name": "龙仙",
      "remark": "豪放可爱女",
      "param_name": "longxian_v3",
      "lang": "中、英"
    },
    {
      "name": "龙铃",
      "remark": "稚气呆板女",
      "param_name": "longling_v3",
      "lang": "中、英"
    },
    {
      "name": "龙闪闪",
      "remark": "戏剧化童声",
      "param_name": "longshanshan_v3",
      "lang": "中、英"
    },
    {
      "name": "龙牛牛",
      "remark": "阳光男童声",
      "param_name": "longniuniu_v3",
      "lang": "中、英"
    },
    {
      "name": "龙嘉欣",
      "remark": "优雅粤语女",
      "param_name": "longjiaxin_v3",
      "lang": "中（粤语）、英"
    },
    {
      "name": "龙嘉怡",
      "remark": "知性粤语女",
      "param_name": "longjiayi_v3",
      "lang": "中（粤语）、英"
    },
    {
      "name": "龙安粤",
      "remark": "欢脱粤语男",
      "param_name": "longanyue_v3",
      "lang": "中（粤语）、英"
    },
    {
      "name": "龙老铁",
      "remark": "东北直率男",
      "param_name": "longlaotie_v3",
      "lang": "中（东北话）、英"
    },
    {
      "name": "龙陕哥",
      "remark": "原味陕北男",
      "param_name": "longshange_v3",
      "lang": "中（陕西话）、英"
    },
    {
      "name": "龙安闽",
      "remark": "清纯萝莉女",
      "param_name": "longanmin_v3",
      "lang": "中（闽南话）、英"
    },
    {
      "name": "龙飞",
      "remark": "热血磁性男",
      "param_name": "longfei_v3",
      "lang": "中、英"
    },
    {
      "name": "龙应笑",
      "remark": "清甜推销女",
      "param_name": "longyingxiao_v3",
      "lang": "中、英"
    },
    {
      "name": "龙应询",
      "remark": "年轻青涩男",
      "param_name": "longyingxun_v3",
      "lang": "中、英"
    },
    {
      "name": "龙应静",
      "remark": "低调冷静女",
      "param_name": "longyingjing_v3",
      "lang": "中、英"
    },
    {
      "name": "龙应聆",
      "remark": "温和共情女",
      "param_name": "longyingling_v3",
      "lang": "中、英"
    },
    {
      "name": "龙应桃",
      "remark": "温柔淡定女",
      "param_name": "longyingtao_v3",
      "lang": "中、英"
    },
    {
      "name": "龙小淳",
      "remark": "知性积极女",
      "param_name": "longxiaochun_v3",
      "lang": "中、英"
    },
    {
      "name": "龙小夏",
      "remark": "沉稳权威女",
      "param_name": "longxiaoxia_v3",
      "lang": "中、英"
    },
    {
      "name": "YUMI",
      "remark": "正经青年女",
      "param_name": "longyumi_v3",
      "lang": "中、英"
    },
    {
      "name": "龙安昀",
      "remark": "居家暖男",
      "param_name": "longanyun_v3",
      "lang": "中、英"
    },
    {
      "name": "龙安温",
      "remark": "优雅知性女",
      "param_name": "longanwen_v3",
      "lang": "中、英"
    },
    {
      "name": "龙安莉",
      "remark": "利落从容女",
      "param_name": "longanli_v3",
      "lang": "中、英"
    },
    {
      "name": "龙安朗",
      "remark": "清爽利落男",
      "param_name": "longanlang_v3",
      "lang": "中、英"
    },
    {
      "name": "龙应沐",
      "remark": "优雅知性女",
      "param_name": "longyingmu_v3",
      "lang": "中、英"
    },
    {
      "name": "龙安台",
      "remark": "嗲甜台湾女",
      "param_name": "longantai_v3",
      "lang": "中、英"
    },
    {
      "name": "龙华",
      "remark": "元气甜美女",
      "param_name": "longhua_v3",
      "lang": "中、英"
    },
    {
      "name": "龙橙",
      "remark": "智慧青年男",
      "param_name": "longcheng_v3",
      "lang": "中、英"
    },
    {
      "name": "龙泽",
      "remark": "温暖元气男",
      "param_name": "longze_v3",
      "lang": "中、英"
    },
    {
      "name": "龙哲",
      "remark": "呆板大暖男",
      "param_name": "longzhe_v3",
      "lang": "中、英"
    },
    {
      "name": "龙颜",
      "remark": "温暖春风女",
      "param_name": "longyan_v3",
      "lang": "中、英"
    },
    {
      "name": "龙星",
      "remark": "温婉邻家女",
      "param_name": "longxing_v3",
      "lang": "中、英"
    },
    {
      "name": "龙天",
      "remark": "磁性理智男",
      "param_name": "longtian_v3",
      "lang": "中、英"
    },
    {
      "name": "龙婉",
      "remark": "细腻柔声女",
      "param_name": "longwan_v3",
      "lang": "中、英"
    },
    {
      "name": "龙嫱",
      "remark": "浪漫风情女",
      "param_name": "longqiang_v3",
      "lang": "中、英"
    },
    {
      "name": "龙菲菲",
      "remark": "甜美娇气女",
      "param_name": "longfeifei_v3",
      "lang": "中、英"
    },
    {
      "name": "龙浩",
      "remark": "多情忧郁男",
      "param_name": "longhao_v3",
      "lang": "中、英"
    },
    {
      "name": "龙安柔",
      "remark": "温柔闺蜜女",
      "param_name": "longanrou_v3",
      "lang": "中、英"
    },
    {
      "name": "龙寒",
      "remark": "温暖痴情男",
      "param_name": "longhan_v3",
      "lang": "中、英"
    },
    {
      "name": "龙安智",
      "remark": "睿智轻熟男",
      "param_name": "longanzhi_v3",
      "lang": "中、英"
    },
    {
      "name": "龙安灵",
      "remark": "思维灵动女",
      "param_name": "longanling_v3",
      "lang": "中、英"
    },
    {
      "name": "龙安雅",
      "remark": "高雅气质女",
      "param_name": "longanya_v3",
      "lang": "中、英"
    },
    {
      "name": "龙安亲",
      "remark": "亲和活泼女",
      "param_name": "longanqin_v3",
      "lang": "中、英"
    },
    {
      "name": "龙妙",
      "remark": "抑扬顿挫女",
      "param_name": "longmiao_v3",
      "lang": "中、英"
    },
    {
      "name": "龙三叔",
      "remark": "沉稳质感男",
      "param_name": "longsanshu_v3",
      "lang": "中、英"
    },
    {
      "name": "龙媛",
      "remark": "温暖治愈女",
      "param_name": "longyuan_v3",
      "lang": "中、英"
    },
    {
      "name": "龙悦",
      "remark": "温暖磁性女",
      "param_name": "longyue_v3",
      "lang": "中、英"
    },
    {
      "name": "龙修",
      "remark": "博才说书男",
      "param_name": "longxiu_v3",
      "lang": "中、英"
    },
    {
      "name": "龙楠",
      "remark": "睿智青年男",
      "param_name": "longnan_v3",
      "lang": "中、英"
    },
    {
      "name": "龙婉君",
      "remark": "细腻柔声女",
      "param_name": "longwanjun_v3",
      "lang": "中、英"
    },
    {
      "name": "龙逸尘",
      "remark": "洒脱活力男",
      "param_name": "longyichen_v3",
      "lang": "中、英"
    },
    {
      "name": "龙老伯",
      "remark": "沧桑岁月爷",
      "param_name": "longlaobo_v3",
      "lang": "中、英"
    },
    {
      "name": "龙老姨",
      "remark": "烟火从容阿姨",
      "param_name": "longlaoyi_v3",
      "lang": "中、英"
    },
    {
      "name": "龙机器",
      "remark": "呆萌机器人",
      "param_name": "longjiqi_v3",
      "lang": "中、英"
    },
    {
      "name": "龙猴哥",
      "remark": "经典猴哥",
      "param_name": "longhouge_v3",
      "lang": "中、英"
    },
    {
      "name": "龙黛玉",
      "remark": "娇率才女音",
      "param_name": "longdaiyu_v3",
      "lang": "中、英"
    },
    {
      "name": "龙安燃",
      "remark": "活泼质感女",
      "param_name": "longanran_v3",
      "lang": "中、英"
    },
    {
      "name": "龙安宣",
      "remark": "经典直播女",
      "param_name": "longanxuan_v3",
      "lang": "中、英"
    },
    {
      "name": "龙硕",
      "remark": "博才干练男",
      "param_name": "longshuo_v3",
      "lang": "中、英"
    },
    {
      "name": "龙书",
      "remark": "沉稳青年男",
      "param_name": "longshu_v3",
      "lang": "中、英"
    },
    {
      "name": "Bella3.0",
      "remark": "精准干练女",
      "param_name": "loongbella_v3",
      "lang": "中、英"
    }
  ]
}', false);
INSERT INTO adi_ai_model (name, title, type, platform, context_window, max_input_tokens, max_output_tokens,
                          response_format_types, is_enable)
VALUES ('qwen3:8b', 'Qwen3-8B(Ollama)', 'text', 'ollama', 131072, 98304, 16384, 'text,json_object', false);
INSERT INTO adi_ai_model (name, title, type, platform, response_format_types, remark, is_free, is_enable)
VALUES ('THUDM/GLM-Z1-9B-0414', 'SiliconFlow-GLM-Z1-9B', 'text', 'siliconflow', 'text,json_object',
        'GLM-Z1-9B-0414 是 GLM 系列的小型模型，仅有 90 亿参数，但保持了开源传统的同时展现出惊人的能力。', true, false);
INSERT INTO adi_ai_model (name, title, type, platform, input_types, remark, is_free, is_enable)
VALUES ('THUDM/GLM-4.1V-9B-Thinking', 'SiliconFlow-Vision', 'vision', 'siliconflow', 'text,image',
        'GLM-4.1V-9B-Thinking 是由智谱 AI 和清华大学 KEG 实验室联合发布的一款开源视觉语言模型（VLM），专为处理复杂的多模态认知任务而设计。该模型基于 GLM-4-9B-0414 基础模型，通过引入“思维链”（Chain-of-Thought）推理机制和采用强化学习策略，显著提升了其跨模态的推理能力和稳定性。作为一个 9B 参数规模的轻量级模型，它在部署效率和性能之间取得了平衡，在 28 项权威评测基准中，有 18 项的表现持平甚至超越了 72B 参数规模的 Qwen-2.5-VL-72B。该模型不仅在图文理解、数学科学推理、视频理解等任务上表现卓越，还支持高达 4K 分辨率的图像和任意宽高比输入',
        true, false);

INSERT INTO adi_ai_model (name, title, type, platform, properties, remark, is_free, is_enable)
VALUES ('Kwai-Kolors/Kolors', 'SiliconFlow-Image', 'image', 'siliconflow', '{
  "image_sizes": [
    "1024x1024",
    "960x1280",
    "768x1024",
    "720x1440",
    "720x1280"
  ],
  "max_images": 4
}',
        'Kolors 是由快手 Kolors 团队开发的基于潜在扩散的大规模文本到图像生成模型。该模型通过数十亿文本-图像对的训练，在视觉质量、复杂语义准确性以及中英文字符渲染方面展现出显著优势。它不仅支持中英文输入，在理解和生成中文特定内容方面也表现出色。',
        true, false);

-- Qwen/Qwen-Image 为收费模型，详细评估后再判断是否要启用
-- INSERT INTO adi_ai_model (name, title, type, platform, properties, remark, is_free, is_enable)
-- VALUES ('Qwen/Qwen-Image', '硅基流动-文生图（qwen）', 'image', 'siliconflow', '{
--   "image_size": [
--     "1328x1328",
--     "1664x928",
--     "928x1664",
--     "1472x1140",
--     "1140x1472",
--     "1584x1056",
--     "1056x1584"
--   ]
-- }',
--         'Qwen-Image 是由阿里巴巴通义千问团队发布的图像生成基础模型，拥有 200 亿参数。该模型在复杂的文本渲染和精确的图像编辑方面取得了显著进展，尤其擅长生成包含高保真度中英文文字的图像。', false, false);

-- test data
INSERT INTO adi_ai_model (name, title, type, platform, response_format_types, is_enable)
VALUES ('THUDM/GLM-Z1-9B-0414', 'openai-compatible-model-test', 'text', 'openai-compatible-platform-test',
        'text,json_object', false);
-- test data end


-- 语音识别
INSERT INTO adi_ai_model (name, title, type, platform, input_types, response_format_types, is_free, is_enable)
VALUES ('FunAudioLLM/SenseVoiceSmall', 'SiliconFlow-ASR', 'asr', 'siliconflow', 'audio', 'text,json_object', true,
        true);
-- 语音合成
INSERT INTO adi_ai_model (name, title, type, platform, input_types, properties, is_enable)
values ('FunAudioLLM/CosyVoice2-0.5B', 'SiliconFlow-TTS', 'tts', 'siliconflow', 'text', '{
  "voices": [
    {
      "name": "fnlp/MOSS-TTSD-v0.5:alex"
    },
    {
      "name": "fnlp/MOSS-TTSD-v0.5:anna"
    },
    {
      "name": "fnlp/MOSS-TTSD-v0.5:bella"
    },
    {
      "name": "fnlp/MOSS-TTSD-v0.5:benjamin"
    },
    {
      "name": "fnlp/MOSS-TTSD-v0.5:charles"
    },
    {
      "name": "fnlp/MOSS-TTSD-v0.5:claire"
    },
    {
      "name": "fnlp/MOSS-TTSD-v0.5:david"
    },
    {
      "name": "fnlp/MOSS-TTSD-v0.5:diana"
    }
  ]
}', true);
