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
VALUES ('conversation_max_num', '50');
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

-- ASR设置
-- 音频文件最大10MB，最大录音时长60秒（TODO）
INSERT INTO adi_sys_config (name, value)
VALUES ('asr_setting',
        '{"model_name":"FunAudioLLM/SenseVoiceSmall","platform":"siliconflow","max_record_duration":60,"max_file_size":10485760}');

-- TTS设置
-- synthesizer_side指定TTS的合成器类型，如: client,server
-- synthesizer_side如果设置为client，表示使用客户端（如浏览器）的tts功能，忽略model_name, platform等参数，免费使用
-- synthesizer_side如果设置为server，表示使用服务端进行语音合成，实际上就是使用各大平台的大语言模型如cosyvoice-v2,FunAudioLLM/CosyVoice2-0.5B等进行合成，付费使用
-- 注意：大语言模型的TTS接口收费不便宜，请提前做好规划
INSERT INTO adi_sys_config (name, value)
VALUES ('tts_setting', '{"synthesizer_side":"client","model_name":"","platform":""}');
-- INSERT INTO adi_sys_config (name, value)
-- VALUES ('tts_setting', '{"synthesizer":"server","model_name":"cosyvoice-v2","platform":"dashscope"}');

-- 模型平台
insert into adi_model_platform (name, title, base_url)
values ('openai', 'OpenAi', 'https://api.openai.com/v1');
insert into adi_model_platform (name, title, base_url)
values ('deepseek', 'DeepSeek深度求索', 'https://api.deepseek.com/v1');
insert into adi_model_platform (name, title, base_url)
values ('dashscope', 'DashScope', 'https://dashscope.aliyuncs.com/api/v1');
insert into adi_model_platform (name, title, base_url)
values ('siliconflow', '硅基流动', 'https://api.siliconflow.cn/v1');
insert into adi_model_platform (name, title, base_url)
values ('ollama', 'ollama', 'http://localhost:11434');
-- 硅基流动的文本模型的api兼容 openai api，本行数据用来测试动态创建的模型平台及模型是否正常使用了 OpenAiCompatibleLLMService 进行请求
insert into adi_model_platform (name, title, base_url, is_openai_api_compatible)
values ('openai-compatible-platform-test', '兼容openai的平台', 'https://api.siliconflow.cn/v1', true);

-- 大语言模型
-- https://api-docs.deepseek.com/zh-cn/quick_start/pricing
INSERT INTO adi_ai_model (name, title, type, platform, context_window, max_input_tokens, max_output_tokens,
                          response_format_types, is_reasoner,
                          is_thinking_closable, is_enable)
VALUES ('deepseek-v4-flash', 'DeepSeek-V4-Flash', 'text', 'deepseek', 1000000, 980000, 384000, 'text,json_object', true, true,
        false);

-- https://platform.openai.com/docs/models/gpt-5-mini
INSERT INTO adi_ai_model (name, title, type, platform, context_window, max_input_tokens, max_output_tokens,
                          response_format_types, remark,
                          is_enable)
VALUES ('gpt-5-mini', 'gpt-5-mini', 'text', 'openai', 400000, 272000, 128000, 'text,json_object',
        'GPT-5 mini is a faster, more cost-efficient version of GPT-5. It''s great for well-defined tasks and precise prompts.',
        false);

-- use gpt-5-mini in place of GPT-3.5 Turbo
-- https://platform.openai.com/docs/models/gpt-3-5-turbo
-- INSERT INTO adi_ai_model (name, title, type, platform, context_window, max_input_tokens, max_output_tokens, is_enable)
-- VALUES ('gpt-3.5-turbo', 'gpt3.5', 'text', 'openai', 16385, 12385, 4096, false);

INSERT INTO adi_ai_model (name, title, type, platform, is_enable)
VALUES ('gpt-image-2', 'GPT-Image-2', 'image', 'openai', false);
INSERT INTO adi_ai_model (name, title, type, platform, max_input_tokens, properties, is_enable)
VALUES ('text-embedding-3-small', 'openai-embedding-small', 'embedding', 'openai', 8191, '{
  "dimension": 1536
}', false);
INSERT INTO adi_ai_model (name, title, type, platform, max_input_tokens, properties, is_enable)
VALUES ('text-embedding-3-large', 'openai-embedding-large', 'embedding', 'openai', 8191, '{
  "dimension": 3072
}', false);
-- https://help.aliyun.com/zh/dashscope/developer-reference/model-introduction?spm=a2c4g.11186623.0.i39
INSERT INTO adi_ai_model (name, title, type, platform, context_window, max_input_tokens, max_output_tokens,
                          response_format_types, is_support_web_search, is_reasoner,
                          is_thinking_closable, is_enable)
VALUES ('qwen-turbo', '通义千问turbo', 'text', 'dashscope', 131072, 98304, 16384, 'text,json_object', true, true, true,
        false);
-- 图片识别
INSERT INTO adi_ai_model (name, title, type, platform, context_window, max_input_tokens, max_output_tokens, input_types,
                          is_enable)
VALUES ('qwen2-vl-7b-instruct', '通义千问-识图', 'vision', 'dashscope', 32768, 16384, 16384, 'text,image', false);
-- https://help.aliyun.com/zh/model-studio/developer-reference/text-to-image-v2-api-reference?spm=a2c4g.11186623.0.i2
-- 通义万相-文生图（wanx2.1-t2i-plus、wanx2.1-t2i-turbo）
INSERT INTO adi_ai_model (name, title, type, platform, is_enable)
VALUES ('wanx2.1-t2i-turbo', '通义万相-文生图', 'image', 'dashscope', false);
-- 通义万相-切换背景
INSERT INTO adi_ai_model (name, title, type, platform, input_types, is_enable)
VALUES ('wanx-background-generation-v2', '通义万相-背景生成', 'image', 'dashscope', 'text,image', false);
-- 通义千问-向量（可选模型名：text-embedding-v1,text-embedding-v2,text-embedding-v3）
INSERT INTO adi_ai_model (name, title, type, platform, max_input_tokens, properties, is_enable)
VALUES ('text-embedding-v1', '通义千问-embedding-v1', 'embedding', 'dashscope', 2048, '{
  "dimension": 1536
}', false);
INSERT INTO adi_ai_model (name, title, type, platform, max_input_tokens, properties, is_enable)
VALUES ('text-embedding-v3', '通义千问-embedding-v3', 'embedding', 'dashscope', 8192, '{
  "dimension": 1024
}', false);
-- 语音识别
-- paraformer-v2 只支持公网可访问的音频文件，如需要传输本地音频文件，请激活使用下面硅基流动的SenseVoiceSmall
INSERT INTO adi_ai_model (name, title, type, platform, input_types, is_enable)
VALUES ('paraformer-v2', '通义-语音识别ASR', 'asr', 'dashscope', 'audio', false);
-- 语音合成
INSERT INTO adi_ai_model (name, title, type, platform, input_types, properties, is_enable)
VALUES ('cosyvoice-v2', '通义-语音合成TTS', 'tts', 'dashscope', 'text', '{
  "voices": [
    {
      "name": "龙应催",
      "remark": "严肃催收男",
      "param_name": "longyingcui",
      "lang": "中、英"
    },
    {
      "name": "龙应答",
      "remark": "开朗高音女",
      "param_name": "longyingda",
      "lang": "中、英"
    },
    {
      "name": "龙应静",
      "remark": "低调冷静女",
      "param_name": "longyingjing",
      "lang": "中、英"
    },
    {
      "name": "龙应严",
      "remark": "义正严辞女",
      "param_name": "longyingyan",
      "lang": "中、英"
    },
    {
      "name": "龙应甜",
      "remark": "温柔甜美女",
      "param_name": "longyingtian",
      "lang": "中、英"
    },
    {
      "name": "龙应冰",
      "remark": "尖锐强势女",
      "param_name": "longyingbing",
      "lang": "中、英"
    },
    {
      "name": "龙应桃",
      "remark": "温柔淡定女",
      "param_name": "longyingtao",
      "lang": "中、英"
    },
    {
      "name": "龙应聆",
      "remark": "温和共情女",
      "param_name": "longyingling",
      "lang": "中、英"
    },
    {
      "name": "YUMI",
      "remark": "正经青年女",
      "param_name": "longyumi_v2",
      "lang": "中、英"
    },
    {
      "name": "龙小淳",
      "remark": "知性积极女",
      "param_name": "longxiaochun_v2",
      "lang": "中、英"
    },
    {
      "name": "龙小夏",
      "remark": "沉稳权威女",
      "param_name": "longxiaoxia_v2",
      "lang": "中、英"
    },
    {
      "name": "龙安燃",
      "remark": "活泼质感女",
      "param_name": "longanran",
      "lang": "中、英"
    },
    {
      "name": "龙安宣",
      "remark": "经典直播女",
      "param_name": "longanxuan",
      "lang": "中、英"
    },
    {
      "name": "龙三叔",
      "remark": "沉稳质感男",
      "param_name": "longsanshu",
      "lang": "中、英"
    },
    {
      "name": "龙修",
      "remark": "博才说书男",
      "param_name": "longxiu_v2",
      "lang": "中、英"
    },
    {
      "name": "龙妙",
      "remark": "抑扬顿挫女",
      "param_name": "longmiao_v2",
      "lang": "中、英"
    },
    {
      "name": "龙悦",
      "remark": "温暖磁性女",
      "param_name": "longyue_v2",
      "lang": "中、英"
    },
    {
      "name": "龙楠",
      "remark": "睿智青年男",
      "param_name": "longnan_v2",
      "lang": "中、英"
    },
    {
      "name": "龙媛",
      "remark": "温暖治愈女",
      "param_name": "longyuan_v2",
      "lang": "中、英"
    },
    {
      "name": "龙安柔",
      "remark": "温柔闺蜜女",
      "param_name": "longanrou",
      "lang": "中、英"
    },
    {
      "name": "龙嫱",
      "remark": "浪漫风情女",
      "param_name": "longqiang_v2",
      "lang": "中、英"
    },
    {
      "name": "龙寒",
      "remark": "温暖痴情男",
      "param_name": "longhan_v2",
      "lang": "中、英"
    },
    {
      "name": "龙星",
      "remark": "温婉邻家女",
      "param_name": "longxing_v2",
      "lang": "中、英"
    },
    {
      "name": "龙华",
      "remark": "元气甜美女",
      "param_name": "longhua_v2",
      "lang": "中、英"
    },
    {
      "name": "龙婉",
      "remark": "积极知性女",
      "param_name": "longwan_v2",
      "lang": "中、英"
    },
    {
      "name": "龙橙",
      "remark": "智慧青年男",
      "param_name": "longcheng_v2",
      "lang": "中、英"
    },
    {
      "name": "龙菲菲",
      "remark": "甜美娇气女",
      "param_name": "longfeifei_v2",
      "lang": "中、英"
    },
    {
      "name": "龙小诚",
      "remark": "磁性低音男",
      "param_name": "longxiaocheng_v2",
      "lang": "中、英"
    },
    {
      "name": "龙哲",
      "remark": "呆板大暖男",
      "param_name": "longzhe_v2",
      "lang": "中、英"
    },
    {
      "name": "龙颜",
      "remark": "温暖春风女",
      "param_name": "longyan_v2",
      "lang": "中、英"
    },
    {
      "name": "龙天",
      "remark": "磁性理智男",
      "param_name": "longtian_v2",
      "lang": "中、英"
    },
    {
      "name": "龙泽",
      "remark": "温暖元气男",
      "param_name": "longze_v2",
      "lang": "中、英"
    },
    {
      "name": "龙邵",
      "remark": "积极向上男",
      "param_name": "longshao_v2",
      "lang": "中、英"
    },
    {
      "name": "龙浩",
      "remark": "多情忧郁男",
      "param_name": "longhao_v2",
      "lang": "中、英"
    },
    {
      "name": "龙深",
      "remark": "实力歌手男",
      "param_name": "kabuleshen_v2",
      "lang": "中、英"
    },
    {
      "name": "龙杰力豆",
      "remark": "阳光顽皮男",
      "param_name": "longjielidou_v2",
      "lang": "中、英"
    },
    {
      "name": "龙铃",
      "remark": "稚气呆板女",
      "param_name": "longling_v2",
      "lang": "中、英"
    },
    {
      "name": "龙可",
      "remark": "懵懂乖乖女",
      "param_name": "longke_v2",
      "lang": "中、英"
    },
    {
      "name": "龙仙",
      "remark": "豪放可爱女",
      "param_name": "longxian_v2",
      "lang": "中、英"
    },
    {
      "name": "龙老铁",
      "remark": "东北直率男",
      "param_name": "longlaotie_v2",
      "lang": "中（东北）、英"
    },
    {
      "name": "龙嘉怡",
      "remark": "知性粤语女",
      "param_name": "longjiayi_v2",
      "lang": "中（粤语）、英"
    },
    {
      "name": "龙桃",
      "remark": "积极粤语女",
      "param_name": "longtao_v2",
      "lang": "中（粤语）、英"
    },
    {
      "name": "龙飞",
      "remark": "热血磁性男",
      "param_name": "longfei_v2",
      "lang": "中、英"
    },
    {
      "name": "李白",
      "remark": "古代诗仙男",
      "param_name": "libai_v2",
      "lang": "中、英"
    },
    {
      "name": "龙津",
      "remark": "优雅温润男",
      "param_name": "longjin_v2",
      "lang": "中、英"
    },
    {
      "name": "龙书",
      "remark": "沉稳青年男",
      "param_name": "longshu_v2",
      "lang": "中、英"
    },
    {
      "name": "Bella2.0",
      "remark": "精准干练女",
      "param_name": "loongbella_v2",
      "lang": "中、英"
    },
    {
      "name": "龙硕",
      "remark": "博才干练男",
      "param_name": "longshuo_v2",
      "lang": "中、英"
    },
    {
      "name": "龙小白",
      "remark": "沉稳播报女",
      "param_name": "longxiaobai_v2",
      "lang": "中、英"
    },
    {
      "name": "龙婧",
      "remark": "典型播音女",
      "param_name": "longjing_v2",
      "lang": "中、英"
    },
    {
      "name": "loongstella",
      "remark": "飒爽利落女",
      "param_name": "loongstella_v2",
      "lang": "中、英"
    },
    {
      "name": "loongeva",
      "remark": "知性英文女",
      "param_name": "loongeva_v2",
      "lang": "英式英文"
    },
    {
      "name": "loongbrian",
      "remark": "沉稳英文男",
      "param_name": "loongbrian_v2",
      "lang": "英式英文"
    },
    {
      "name": "loongluna",
      "remark": "英式英文女",
      "param_name": "loongluna_v2",
      "lang": "英式英文"
    },
    {
      "name": "loongluca",
      "remark": "英式英文男",
      "param_name": "loongluca_v2",
      "lang": "英式英文"
    },
    {
      "name": "loongemily",
      "remark": "英式英文女",
      "param_name": "loongemily_v2",
      "lang": "英式英文"
    },
    {
      "name": "loongeric",
      "remark": "英式英文男",
      "param_name": "loongeric_v2",
      "lang": "英式英文"
    },
    {
      "name": "loongabby",
      "remark": "美式英文女",
      "param_name": "loongabby_v2",
      "lang": "美式英文"
    },
    {
      "name": "loongannie",
      "remark": "美式英文女",
      "param_name": "loongannie_v2",
      "lang": "美式英文"
    },
    {
      "name": "loongandy",
      "remark": "美式英文男",
      "param_name": "loongandy_v2",
      "lang": "美式英文"
    },
    {
      "name": "loongava",
      "remark": "美式英文女",
      "param_name": "loongava_v2",
      "lang": "美式英文"
    },
    {
      "name": "loongbeth",
      "remark": "美式英文女",
      "param_name": "loongbeth_v2",
      "lang": "美式英文"
    },
    {
      "name": "loongbetty",
      "remark": "美式英文女",
      "param_name": "loongbetty_v2",
      "lang": "美式英文"
    },
    {
      "name": "loongcindy",
      "remark": "美式英文女",
      "param_name": "loongcindy_v2",
      "lang": "美式英文"
    },
    {
      "name": "loongcally",
      "remark": "美式英文女",
      "param_name": "loongcally_v2",
      "lang": "美式英文"
    },
    {
      "name": "loongdavid",
      "remark": "美式英文男",
      "param_name": "loongdavid_v2",
      "lang": "美式英文"
    },
    {
      "name": "loongdonna",
      "remark": "美式英文女",
      "param_name": "loongdonna_v2",
      "lang": "美式英文"
    },
    {
      "name": "loongkyong",
      "remark": "韩语女",
      "param_name": "loongkyong_v2",
      "lang": "韩语"
    },
    {
      "name": "loongtomoka",
      "remark": "日语女",
      "param_name": "loongtomoka_v2",
      "lang": "日语"
    },
    {
      "name": "loongtomoya",
      "remark": "日语男",
      "param_name": "loongtomoya_v2",
      "lang": "日语"
    }
  ]
}', false);
INSERT INTO adi_ai_model (name, title, type, platform, is_enable)
VALUES ('tinydolphin', 'ollama-tinydolphin', 'text', 'ollama', false);
INSERT INTO adi_ai_model (name, title, type, platform, response_format_types, remark, is_free, is_enable)
VALUES ('THUDM/GLM-Z1-9B-0414', '硅基流动-GLM-Z1-9B', 'text', 'siliconflow', 'text,json_object',
        'GLM-Z1-9B-0414 是 GLM 系列的小型模型，仅有 90 亿参数，但保持了开源传统的同时展现出惊人的能力。', true, false);
INSERT INTO adi_ai_model (name, title, type, platform, input_types, remark, is_free, is_enable)
VALUES ('THUDM/GLM-4.1V-9B-Thinking', '硅基流动-识图', 'vision', 'siliconflow', 'text,image',
        'GLM-4.1V-9B-Thinking 是由智谱 AI 和清华大学 KEG 实验室联合发布的一款开源视觉语言模型（VLM），专为处理复杂的多模态认知任务而设计。该模型基于 GLM-4-9B-0414 基础模型，通过引入“思维链”（Chain-of-Thought）推理机制和采用强化学习策略，显著提升了其跨模态的推理能力和稳定性。作为一个 9B 参数规模的轻量级模型，它在部署效率和性能之间取得了平衡，在 28 项权威评测基准中，有 18 项的表现持平甚至超越了 72B 参数规模的 Qwen-2.5-VL-72B。该模型不仅在图文理解、数学科学推理、视频理解等任务上表现卓越，还支持高达 4K 分辨率的图像和任意宽高比输入',
        true, false);

INSERT INTO adi_ai_model (name, title, type, platform, properties, remark, is_free, is_enable)
VALUES ('Kwai-Kolors/Kolors', '硅基流动-文生图', 'image', 'siliconflow', '{
  "image_sizes": [
    "1024x1024",
    "960x1280",
    "768x1024",
    "720x1440",
    "720x1280"
  ]
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
VALUES ('FunAudioLLM/SenseVoiceSmall', '硅基流动-语音识别', 'asr', 'siliconflow', 'audio', 'text,json_object', true,
        true);
-- 语音合成
INSERT INTO adi_ai_model (name, title, type, platform, input_types, properties, is_enable)
values ('FunAudioLLM/CosyVoice2-0.5B', '硅基流动-语音合成', 'tts', 'siliconflow', 'text', '{
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
