-- ============================================================
-- Migration: Add split strategy fields to adi_knowledge_base
-- Description: Support configurable chunking strategies including
--   recursive, paragraph, line, sentence, and custom separator.
--   Also make segment size configurable (previously hardcoded 1000).
-- ============================================================

ALTER TABLE adi_knowledge_base ADD COLUMN ingest_split_strategy  varchar(20)  DEFAULT 'recursive' NOT NULL;
ALTER TABLE adi_knowledge_base ADD COLUMN ingest_max_segment_size int          DEFAULT 1000      NOT NULL;
ALTER TABLE adi_knowledge_base ADD COLUMN ingest_custom_separator varchar(100) DEFAULT ''         NOT NULL;

COMMENT ON COLUMN adi_knowledge_base.ingest_split_strategy  IS 'Split strategy: recursive/paragraph/line/sentence/custom';
COMMENT ON COLUMN adi_knowledge_base.ingest_max_segment_size IS 'Max segment size in tokens when chunking documents';
COMMENT ON COLUMN adi_knowledge_base.ingest_custom_separator  IS 'Custom separator for splitting, only used when strategy is custom';

-- ============================================================
-- Migration: Update outdated AI models
-- Description:
--   1. Replace tinydolphin (Ollama) with qwen3:8b
--   2. Replace cosyvoice-v2 (DashScope TTS) with cosyvoice-v3-flash
--   3. Update tts_setting config to reference cosyvoice-v3-flash
-- ============================================================

-- 1. Replace tinydolphin with qwen3:8b (Ollama text model)
UPDATE adi_ai_model
SET name = 'qwen3:8b',
    title = 'Qwen3-8B(Ollama)',
    context_window = 131072,
    max_input_tokens = 98304,
    max_output_tokens = 16384,
    response_format_types = 'text,json_object'
WHERE name = 'tinydolphin' AND platform = 'ollama';

-- 2. Replace cosyvoice-v2 with cosyvoice-v3-flash (DashScope TTS)
UPDATE adi_ai_model
SET name = 'cosyvoice-v3-flash',
    title = 'Qwen-TTS',
    properties = '{
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
}'::jsonb
WHERE name = 'cosyvoice-v2' AND platform = 'dashscope';

-- 3. Update tts_setting config references from cosyvoice-v2 to cosyvoice-v3-flash
UPDATE adi_sys_config
SET value = REPLACE(value, 'cosyvoice-v2', 'cosyvoice-v3-flash')
WHERE name = 'tts_setting' AND value LIKE '%cosyvoice-v2%';
