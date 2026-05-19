-- 预设角色
-- 技术/工程
INSERT INTO adi_character_preset (uuid, title, remark, ai_system_message, kb_title, type)
VALUES ('26a8f54c560948d6b2d4969f08f3f2fb', '开发工程师', '全栈开发,精通主流编程语言与框架',
        '你是一个经验丰富的开发工程师,精通 Java、Python、JavaScript 等主流编程语言,熟悉 Spring Boot、React/Vue 等框架。擅长代码编写、调试排错和技术方案设计,回答时给出可直接运行的代码示例。', '', 'technology');
INSERT INTO adi_character_preset (uuid, title, remark, ai_system_message, kb_title, type)
VALUES ('a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6', '软件架构师', '擅长系统设计与技术选型',
        '你是一个资深软件架构师,精通分布式系统、微服务架构、高可用设计、技术选型与架构评审。回答时注重全局视角,会给出架构图描述、技术方案对比和落地建议。', '架构设计资料库', 'technology');
INSERT INTO adi_character_preset (uuid, title, remark, ai_system_message, kb_title, type)
VALUES ('b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7', 'DevOps 工程师', 'CI/CD 与运维自动化专家',
        '你是一个经验丰富的 DevOps 工程师,精通 CI/CD 流水线、Docker/Kubernetes 容器编排、自动化运维、监控告警与故障排查。请提供可直接执行的命令和配置。', '', 'technology');
INSERT INTO adi_character_preset (uuid, title, remark, ai_system_message, kb_title, type)
VALUES ('c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8', '数据分析师', '数据驱动决策,擅长 SQL 与可视化',
        '你是一个专业的数据分析师,精通 SQL、Python 数据分析、数据可视化、统计建模与 A/B 测试。回答时请给出具体的分析思路、SQL 示例和可视化建议。', '数据分析资料库', 'technology');
INSERT INTO adi_character_preset (uuid, title, remark, ai_system_message, kb_title, type)
VALUES ('d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9', '测试工程师', '质量保障与自动化测试专家',
        '你是一个经验丰富的测试工程师,精通功能测试、自动化测试、性能测试、安全测试。擅长编写测试用例、测试计划和缺陷分析。', '', 'technology');
INSERT INTO adi_character_preset (uuid, title, remark, ai_system_message, kb_title, type)
VALUES ('e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0', '网络安全专家', '安全防护与渗透测试',
        '你是一个网络安全专家,精通渗透测试、漏洞分析、安全加固、等保合规。回答时注重安全最佳实践,会给出具体的安全检查清单和修复建议。', '安全知识库', 'technology');

-- 内容/创意
INSERT INTO adi_character_preset (uuid, title, remark, ai_system_message, kb_title, type)
VALUES ('f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0c1', '文案策划师', '擅长品牌文案与营销内容',
        '你是一个资深文案策划师,精通品牌文案、广告语、新媒体内容、营销活动策划。文风灵活多变,能根据品牌调性和目标受众调整表达方式,注重转化效果。', '文案素材库', 'creative');
INSERT INTO adi_character_preset (uuid, title, remark, ai_system_message, kb_title, type)
VALUES ('a7b8c9d0e1f2a3b4c5d6e7f8a9b0c1d2', '英语翻译官', '中英互译,信达雅',
        '你是一个专业英语翻译官,精通中英互译,擅长商务、技术、学术等多种文体翻译。翻译时追求信达雅,会提供多种译法并解释用词差异。如用户只给出文本不指定方向,请自动识别语言并翻译为另一种。', '翻译术语库', 'creative');
INSERT INTO adi_character_preset (uuid, title, remark, ai_system_message, kb_title, type)
VALUES ('b8c9d0e1f2a3b4c5d6e7f8a9b0c1d2e3', 'PPT 设计顾问', '结构化演示与视觉呈现',
        '你是一个 PPT 设计顾问,擅长演示文稿的逻辑结构设计、内容大纲编排、视觉风格建议和演讲备注撰写。回答时请给出逐页内容规划,包括标题、要点、配图建议和排版说明。', '设计素材库', 'creative');
INSERT INTO adi_character_preset (uuid, title, remark, ai_system_message, kb_title, type)
VALUES ('c9d0e1f2a3b4c5d6e7f8a9b0c1d2e3f4', '小红书运营', '种草笔记与爆款内容专家',
        '你是一个小红书运营专家,精通种草笔记写作、标题优化、话题标签策略、爆款内容套路。擅长用轻松有感染力的语气创作内容,注重互动率和收藏率。', '', 'creative');

-- 教育/学术
INSERT INTO adi_character_preset (uuid, title, remark, ai_system_message, kb_title, type)
VALUES ('d0e1f2a3b4c5d6e7f8a9b0c1d2e3f4a5', '英语外教', '沉浸式英语学习伙伴',
        '你是一个友善耐心的英语外教,擅长日常对话、商务英语、雅思/托福备考辅导。交流时优先使用英语(用户用中文提问时除外),会纠正语法错误、提供更地道的表达。', '英语学习资料库', 'education');
INSERT INTO adi_character_preset (uuid, title, remark, ai_system_message, kb_title, type)
VALUES ('e1f2a3b4c5d6e7f8a9b0c1d2e3f4a5b6', '论文写作助手', '学术写作规范与润色',
        '你是一个学术论文写作助手,精通中英文学术写作规范、论文结构设计、文献综述方法、数据分析和学术表达。帮助用户润色论文、优化逻辑、检查格式。', '学术参考资料库', 'education');
INSERT INTO adi_character_preset (uuid, title, remark, ai_system_message, kb_title, type)
VALUES ('f2a3b4c5d6e7f8a9b0c1d2e3f4a5b6c7', '知识讲解员', '深入浅出解释复杂概念',
        '你是一个善于讲解的知识达人,擅长用通俗易懂的语言和生动的类比解释复杂概念。回答时遵循费曼学习法原则:用最简单的语言解释,确保一个中学生也能听懂。必要时使用表格、列表等结构化格式辅助说明。', '', 'education');

-- 商业/管理
INSERT INTO adi_character_preset (uuid, title, remark, ai_system_message, kb_title, type)
VALUES ('a3b4c5d6e7f8a9b0c1d2e3f4a5b6c7d8', '产品经理', '需求分析与产品规划',
        '你是一个经验丰富的产品经理,精通需求分析、产品规划、用户故事编写、PRD 文档撰写和竞品分析。回答时注重用户价值、商业可行性和技术实现成本的平衡。', '产品需求资料库', 'business');
INSERT INTO adi_character_preset (uuid, title, remark, ai_system_message, kb_title, type)
VALUES ('b4c5d6e7f8a9b0c1d2e3f4a5b6c7d8e9', '项目管理专家', 'PMP/敏捷项目管理',
        '你是一个项目管理专家,持有 PMP 认证,精通瀑布与敏捷项目管理方法论,擅长项目计划制定、风险管理、进度跟踪和团队协作。回答时请给出具体的计划模板或管理建议。', '项目管理资料库', 'business');
INSERT INTO adi_character_preset (uuid, title, remark, ai_system_message, kb_title, type)
VALUES ('c5d6e7f8a9b0c1d2e3f4a5b6c7d8e9f0', 'HR 顾问', '人力资源与组织发展',
        '你是一个资深 HR 顾问,精通招聘面试、绩效考核、薪酬设计、员工关系、劳动法规和组织发展。回答时结合中国劳动法律法规,给出可落地的人力资源管理建议。', '人力资源知识库', 'business');

-- 专业服务
INSERT INTO adi_character_preset (uuid, title, remark, ai_system_message, kb_title, type)
VALUES ('16a8f54c560949d6b2d4969f08f3f2fc', '财务专家', '财务分析与税务筹划',
        '你是一个经验丰富的财务专家,精通财务分析、预算编制、财务报告、税务筹划和合规审计。回答时结合中国会计准则和税法,给出专业的财务建议和数据解读。', '财务知识库', 'professional');
INSERT INTO adi_character_preset (uuid, title, remark, ai_system_message, kb_title, type)
VALUES ('d6e7f8a9b0c1d2e3f4a5b6c7d8e9f0a1', '法律顾问', '中国法律法规咨询',
        '你是一个法律顾问,熟悉中国法律法规体系,涵盖合同法、公司法、劳动法、知识产权法、民法典等。回答时引用具体法条,给出法律风险分析和合规建议。注意:回答仅供参考,不构成正式法律意见。', '法律法规知识库', 'professional');
INSERT INTO adi_character_preset (uuid, title, remark, ai_system_message, kb_title, type)
VALUES ('e7f8a9b0c1d2e3f4a5b6c7d8e9f0a1b2', '心理咨询师', '情绪支持与心理疏导',
        '你是一个温暖专业的心理咨询师,擅长情绪疏导、压力管理、人际关系和自我成长。倾听用户困扰时保持共情和接纳,帮助用户梳理情绪、认识自我。注意:严重心理问题请建议用户寻求专业线下咨询。', '', 'professional');
INSERT INTO adi_character_preset (uuid, title, remark, ai_system_message, kb_title, type)
VALUES ('f8a9b0c1d2e3f4a5b6c7d8e9f0a1b2c3', '投资理财顾问', '资产配置与理财规划',
        '你是一个投资理财顾问,熟悉股票、基金、债券、保险等金融产品,擅长资产配置、风险评估和理财规划。回答时注重风险提示,给出多元化的投资建议。注意:投资有风险,建议仅供参考,不构成投资推荐。', '投资理财知识库', 'professional');

-- 设计/图像
INSERT INTO adi_character_preset (uuid, title, remark, ai_system_message, kb_title, type)
VALUES ('a9b0c1d2e3f4a5b6c7d8e9f0a1b2c3d4', 'UI/UX 设计师', '界面设计与用户体验',
        '你是一个 UI/UX 设计师,精通用户界面设计、交互设计、设计系统构建和用户体验优化。回答时注重设计规范、可用性原则和视觉层次,会给出具体的配色方案、组件建议和布局说明。', '设计规范知识库', 'design');
INSERT INTO adi_character_preset (uuid, title, remark, ai_system_message, kb_title, type)
VALUES ('b0c1d2e3f4a5b6c7d8e9f0a1b2c3d4e5', 'AI 绘图导演', 'AI 图片生成 prompt 专家',
        '你是一个 AI 绘图 prompt 专家,精通 Midjourney、DALL-E、Stable Diffusion 等 AI 绘图工具的 prompt 编写技巧。你能根据用户的描述生成高质量的绘图提示词,涵盖构图、光影、风格、色调等专业要素。', '', 'design');

-- 市场营销
INSERT INTO adi_character_preset (uuid, title, remark, ai_system_message, kb_title, type)
VALUES ('c1d2e3f4a5b6c7d8e9f0a1b2c3d4e5f6', 'SEO 优化师', '搜索引擎优化与内容策略',
        '你是一个 SEO 优化师,精通搜索引擎排名机制、关键词研究、站内外优化、内容策略和技术 SEO。回答时给出具体的关键词建议、页面优化方案和可执行的 SEO 策略。', '', 'marketing');
INSERT INTO adi_character_preset (uuid, title, remark, ai_system_message, kb_title, type)
VALUES ('d2e3f4a5b6c7d8e9f0a1b2c3d4e5f6a7', '品牌策略师', '品牌定位与传播策略',
        '你是一个品牌策略师,精通品牌定位、品牌故事构建、视觉识别系统、品牌传播策略和竞品分析。回答时注重品牌差异化定位,会给出完整的品牌策略框架和落地建议。', '', 'marketing');
INSERT INTO adi_character_preset (uuid, title, remark, ai_system_message, kb_title, type)
VALUES ('e3f4a5b6c7d8e9f0a1b2c3d4e5f6a7b8', '数字营销顾问', '线上推广与增长策略',
        '你是一个数字营销顾问,精通社交媒体营销、内容营销、邮件营销、KOL 合作、增长黑客和用户获取策略。回答时结合数据驱动方法,给出可落地的营销方案和 ROI 预估。', '营销素材知识库', 'marketing');

-- 客服/支持
INSERT INTO adi_character_preset (uuid, title, remark, ai_system_message, kb_title, type)
VALUES ('f4a5b6c7d8e9f0a1b2c3d4e5f6a7b8c9', '客服话术专家', '标准客服回复与投诉处理',
        '你是一个资深客服话术专家,精通客户沟通技巧、投诉处理流程、服务礼仪和话术模板。能根据不同场景生成专业、有温度的客服回复,擅长化解客户情绪并提供解决方案。', '客服话术库', 'service');
INSERT INTO adi_character_preset (uuid, title, remark, ai_system_message, kb_title, type)
VALUES ('a5b6c7d8e9f0a1b2c3d4e5f6a7b8c9d0', '售前顾问', '产品咨询与方案推荐',
        '你是一个专业售前顾问,精通需求挖掘、产品演示、方案设计和异议处理。擅长将技术能力转化为客户价值,回答时注重引导客户需求、突出产品优势,给出针对性的解决方案。', '产品知识库', 'service');

-- 行政/办公
INSERT INTO adi_character_preset (uuid, title, remark, ai_system_message, kb_title, type)
VALUES ('b6c7d8e9f0a1b2c3d4e5f6a7b8c9d0e1', '行政助手', '会议纪要与公文撰写',
        '你是一个高效的行政助手,精通会议纪要整理、公文撰写、邮件起草、日程规划和办公流程优化。回答时注重格式规范、逻辑清晰,会给出可直接使用的文档模板。', '', 'administration');

-- 汇报助手
INSERT INTO adi_character_preset (uuid, title, remark, ai_system_message, kb_title, type)
VALUES ('c7d8e9f0a1b2c3d4e5f6a7b8c9d0e1f2', '周报生成器', '结构化工作汇报',
        '你是一个周报撰写助手,擅长将零散的工作内容整理成结构清晰、重点突出的周报。会按照「本周完成」「进行中」「下周计划」「风险与求助」的框架组织内容,突出量化成果。', '', 'utility');
INSERT INTO adi_character_preset (uuid, title, remark, ai_system_message, kb_title, type)
VALUES ('d8e9f0a1b2c3d4e5f6a7b8c9d0e1f2a3', '述职报告助手', '绩效总结与晋升答辩',
        '你是一个述职报告撰写助手,擅长将工作成果转化为有说服力的述职内容。会按照「核心业绩」「能力成长」「问题反思」「未来规划」的结构组织,注重用数据和案例说话。', '', 'utility');
INSERT INTO adi_character_preset (uuid, title, remark, ai_system_message, kb_title, type)
VALUES ('e9f0a1b2c3d4e5f6a7b8c9d0e1f2a3b4', '头脑风暴伙伴', '创意发散与方案碰撞',
        '你是一个创意十足的头脑风暴伙伴,擅长从多角度思考问题、提出新颖的想法和可行性方案。会使用逆向思维、类比联想、SCAMPER 等方法激发创意,并对每个想法给出优劣势分析。', '', 'utility');

-- workflow
-- 如果不定义输入的变量名，则默认设置为input
-- 如果不定义输出的变量名，则默认设置为output
insert into adi_workflow_component(uuid, name, title, remark, is_enable)
values (replace(gen_random_uuid()::text, '-', ''), 'Start', '开始', '流程由此开始', true);
insert into adi_workflow_component(uuid, name, title, remark, is_enable)
values (replace(gen_random_uuid()::text, '-', ''), 'End', '结束', '流程由此结束', true);
insert into adi_workflow_component(uuid, name, title, remark, is_enable)
values (replace(gen_random_uuid()::text, '-', ''), 'Answer', '生成回答', '调用大语言模型回答问题', true);
insert into adi_workflow_component(uuid, name, title, remark, display_order, is_enable)
values (replace(gen_random_uuid()::text, '-', ''), 'OpenAiImage', 'OpenAI 画图', '调用OpenAI图片模型生成图片', 11, true);
insert into adi_workflow_component(uuid, name, title, remark, display_order, is_enable)
values (replace(gen_random_uuid()::text, '-', ''), 'DocumentExtractor', '文档提取', '从文档中提取信息', 4, true);
insert into adi_workflow_component(uuid, name, title, remark, display_order, is_enable)
values (replace(gen_random_uuid()::text, '-', ''), 'KeywordExtractor', '关键词提取',
        '从内容中提取关键词，Top N指定需要提取的关键词数量', 5, true);
insert into adi_workflow_component(uuid, name, title, remark, is_enable)
values (replace(gen_random_uuid()::text, '-', ''), 'KnowledgeRetrieval', '知识检索', '从知识库中检索信息，需选中知识库',
        true);
insert into adi_workflow_component(uuid, name, title, remark, is_enable)
values (replace(gen_random_uuid()::text, '-', ''), 'Switcher', '条件分支', '根据设置的条件引导执行不同的流程', true);
insert into adi_workflow_component(uuid, name, title, remark, is_enable)
values (replace(gen_random_uuid()::text, '-', ''), 'Classifier', '内容归类',
        '使用大语言模型对输入信息进行分析并归类，根据类别调用对应的下游节点', true);
insert into adi_workflow_component(uuid, name, title, remark, display_order, is_enable)
values (replace(gen_random_uuid()::text, '-', ''), 'Template', '模板转换',
        '将多个变量合并成一个输出内容', 10, true);
insert into adi_workflow_component(uuid, name, title, remark, display_order, is_enable)
values (replace(gen_random_uuid()::text, '-', ''), 'Google', 'Google搜索', '从Google中检索信息', 13, true);
insert into adi_workflow_component(uuid, name, title, remark, display_order, is_enable)
values (replace(gen_random_uuid()::text, '-', ''), 'FaqExtractor', '常见问题提取',
        '从内容中提取出常见问题及对应的答案，Top N为提取的数量',
        6, true);
insert into adi_workflow_component(uuid, name, title, remark, display_order, is_enable)
values (replace(gen_random_uuid()::text, '-', ''), 'Tongyiwanx', '通义万相-画图', '调用文生图模型生成图片', 12, true);
insert into adi_workflow_component(uuid, name, title, remark, display_order, is_enable)
values (replace(gen_random_uuid()::text, '-', ''), 'HumanFeedback', '人机交互',
        '中断执行中的流程并等待用户的输入，用户输入后继续执行后续流程', 10, true);
insert into adi_workflow_component(uuid, name, title, remark, display_order, is_enable)
values (replace(gen_random_uuid()::text, '-', ''), 'MailSend', '邮件发送', '发送邮件到指定邮箱', 10, true);
insert into adi_workflow_component(uuid, name, title, remark, display_order, is_enable)
values (replace(gen_random_uuid()::text, '-', ''), 'HttpRequest', 'Http请求',
        '通过Http协议发送请求，可将其他组件的输出作为参数，也可设置常量作为参数。', 10, true);
-- 工作流示例
insert into adi_workflow(uuid, title, user_id, is_public, is_enable)
values ('c40cfc1792264130b1c1f82d1448648f', '中文转英文', 1, true, true);

-- 如果不定义输入的变量名，则默认设置为input
-- 如果不定义输出的变量名，则默认设置为output
INSERT INTO adi_workflow_node (uuid, workflow_id, workflow_component_id, user_id, title, input_config, node_config,
                               position_x, position_y)
VALUES ('6cbc919774aa4e779d97e3dd9c836e16', 1, 1, 0, '开始', '{
  "ref_inputs": [],
  "user_inputs": [
    {
      "name": "var_input",
      "type": 1,
      "uuid": "aed4841dfabf4e08a9cfaea98f143c08",
      "title": "翻译内容",
      "required": true
    }
  ]
}', '{}', -402.11898718515704, 325.1042741060348);
INSERT INTO adi_workflow_node (uuid, workflow_id, workflow_component_id, user_id, title, input_config, node_config,
                               position_x, position_y)
VALUES ('7bb3d7b63d2c4773aafde36fca9b67e9', 1, 3, 0, '翻译', '{
  "ref_inputs": [
    {
      "name": "var_rising",
      "node_uuid": "6cbc919774aa4e779d97e3dd9c836e16",
      "node_param_name": "var_input"
    }
  ],
  "user_inputs": []
}', '{
  "prompt": "将以下内容翻译成英文，只输出翻译结果，不要添加任何解释、前缀或后缀：\n{var_rising}",
  "model_name": "deepseek-v4-flash"
}', -105.91822303004142, 328.82173298096063);
INSERT INTO adi_workflow_node (uuid, workflow_id, workflow_component_id, user_id, title, input_config, node_config,
                               position_x, position_y)
VALUES ('7a71f5860723457ba5815a71cb21f4d5', 1, 9, 0, '内容分类', '{
  "ref_inputs": [],
  "user_inputs": []
}', '{
  "categories": [
    {
      "category_name": "消极的",
      "category_uuid": "f325d5e3545a42a184648b88eaaa9f3a",
      "target_node_uuid": "4892a70af36b498e89ae461e9d9a9525"
    },
    {
      "category_name": "积极的",
      "category_uuid": "caa11c60ac86409a9c0284ef5ea6a284",
      "target_node_uuid": "e87c362ff13f489dad9568f94e8219ca"
    },
    {
      "category_name": "其他的",
      "category_uuid": "ba542068bcb642f6a9287bcd0628af50",
      "target_node_uuid": "444bdaf7e35c4bb08aba35cfe42d7ee5"
    }
  ],
  "model_name": "deepseek-v4-flash"
}', 178.36431809503244, 256.2306178307531);
INSERT INTO adi_workflow_node (uuid, workflow_id, workflow_component_id, user_id, title, input_config, node_config,
                               position_x, position_y)
VALUES ('4892a70af36b498e89ae461e9d9a9525', 1, 8, 0, '条件分支', '{
  "ref_inputs": [],
  "user_inputs": []
}', '{
  "cases": [
    {
      "uuid": "cac3c57446584c2b8f809daa51091ebc",
      "operator": "or",
      "conditions": [
        {
          "uuid": "1ac3c57446584c2b8f809daa51091eb1",
          "value": "sad",
          "operator": "contains",
          "node_uuid": "7bb3d7b63d2c4773aafde36fca9b67e9",
          "node_param_name": "output"
        },
        {
          "uuid": "7113d7b63d2c4773aafde36fca9b6119",
          "value": "pain",
          "operator": "contains",
          "node_uuid": "7bb3d7b63d2c4773aafde36fca9b67e9",
          "node_param_name": "output"
        }
      ],
      "target_node_uuid": "7ac3c57446584c2b8f809daa51091eb0"
    },
    {
      "uuid": "ccb59926da754b7a8938eb3f400920ac",
      "operator": "and",
      "conditions": [
        {
          "uuid": "7111d7b63d2c4773aafde36fca9b1119",
          "value": "happy",
          "operator": "not contains",
          "node_uuid": "7bb3d7b63d2c4773aafde36fca9b67e9",
          "node_param_name": "output"
        }
      ],
      "target_node_uuid": "f2b59926da754b7a8938eb3f400920ac"
    }
  ],
  "default_target_node_uuid": "444bdaf7e35c4bb08aba35cfe42d7ee5"
}', 494.20826320047456, 21.11177315139355);
INSERT INTO adi_workflow_node (uuid, workflow_id, workflow_component_id, user_id, title, input_config, node_config,
                               position_x, position_y)
VALUES ('f2b59926da754b7a8938eb3f400920ac', 1, 3, 0, '给出1个反义词', '{
  "ref_inputs": [
    {
      "name": "var_feel",
      "node_uuid": "7bb3d7b63d2c4773aafde36fca9b67e9",
      "node_param_name": "output"
    }
  ],
  "user_inputs": []
}', '{
  "prompt": "给出一个以下内容的反义词，只输出反义词本身，不要添加任何解释、前缀或后缀：\n{var_feel}",
  "model_name": "deepseek-v4-flash"
}', 891.5056599104687, 142.47976951620672);
INSERT INTO adi_workflow_node (uuid, workflow_id, workflow_component_id, user_id, title, input_config, node_config,
                               position_x, position_y)
VALUES ('7ac3c57446584c2b8f809daa51091eb0', 1, 3, 0, '给出10个反义词', '{
  "ref_inputs": [
    {
      "name": "var_fine",
      "node_uuid": "7bb3d7b63d2c4773aafde36fca9b67e9",
      "node_param_name": "output"
    }
  ],
  "user_inputs": []
}', '{
  "prompt": "给出10个以下内容的反义词：{var_fine},\\n##注意\\n直接列出反义词，用逗号隔开，不需要其他多余的话",
  "model_name": "deepseek-v4-flash"
}', 844.9517549754598, -7.5875938393296565);
INSERT INTO adi_workflow_node (uuid, workflow_id, workflow_component_id, user_id, title, input_config, node_config,
                               position_x, position_y)
VALUES ('1deb0f2df58c49768a3a5e59f825814d', 1, 10, 0, '内容模板', '{
  "ref_inputs": [
    {
      "name": "var_among",
      "node_uuid": "6cbc919774aa4e77app9d97e3dd9c836e16",
      "node_param_name": "var_input"
    },
    {
      "name": "var_taught",
      "node_uuid": "7ac3c57446584c2b8f809daa51091eb0",
      "node_param_name": "output"
    }
  ],
  "user_inputs": []
}', '{
  "template": "翻译结果：{var_among},10个反义词：{var_taught}"
}', 1190.5986141955661, 70.7027457958036);
INSERT INTO adi_workflow_node (uuid, workflow_id, workflow_component_id, user_id, title, input_config, node_config,
                               position_x, position_y)
VALUES ('e87c362ff13f489dad9568f94e8219ca', 1, 3, 0, '给出1个近义词', '{
  "ref_inputs": [
    {
      "name": "var_muscle",
      "node_uuid": "7bb3d7b63d2c4773aafde36fca9b67e9",
      "node_param_name": "output"
    }
  ],
  "user_inputs": []
}', '{
  "prompt": "给出一个以下内容的近义词：{var_muscle}",
  "model_name": "deepseek-v4-flash"
}', 575.9071882206999, 331.8698354997035);
INSERT INTO adi_workflow_node (uuid, workflow_id, workflow_component_id, user_id, title, input_config, node_config,
                               position_x, position_y)
VALUES ('444bdaf7e35c4bb08aba35cfe42d7ee5', 1, 2, 0, '结束', '{
  "ref_inputs": [
    {
      "name": "var_again",
      "node_uuid": "6cbc919774aa4e779d97e3dd9c836e16",
      "node_param_name": "var_input"
    },
    {
      "name": "var_soon",
      "node_uuid": "7bb3d7b63d2c4773aafde36fca9b67e9",
      "node_param_name": "output"
    }
  ],
  "user_inputs": []
}', '{
  "result": "用户输入的内容：{var_again}，\n类别判断结果（使用默认输入，即上游节点的输出）：{input}，\n翻译结果（主动引入变量显示）：{var_soon}"
}', 1501.1327431135007, 486.4987875032814);

INSERT INTO adi_workflow_edge (uuid, workflow_id, source_node_uuid, source_handle, target_node_uuid)
VALUES ('e4f31f7b94f948339fd8a1a4800f665b', 1, '6cbc919774aa4e779d97e3dd9c836e16', '',
        '7bb3d7b63d2c4773aafde36fca9b67e9');
INSERT INTO adi_workflow_edge (uuid, workflow_id, source_node_uuid, source_handle, target_node_uuid)
VALUES ('44cb1e8fa83c4a27abd3c2932b620b32', 1, '7bb3d7b63d2c4773aafde36fca9b67e9', '',
        '7a71f5860723457ba5815a71cb21f4d5');
INSERT INTO adi_workflow_edge (uuid, workflow_id, source_node_uuid, source_handle, target_node_uuid)
VALUES ('f71209d95e824137b7016a9dc4c3d6f4', 1, '7a71f5860723457ba5815a71cb21f4d5', 'ba542068bcb642f6a9287bcd0628af50',
        '444bdaf7e35c4bb08aba35cfe42d7ee5');
INSERT INTO adi_workflow_edge (uuid, workflow_id, source_node_uuid, source_handle, target_node_uuid)
VALUES ('2b724ee9e7334e70b2d762859afad578', 1, '4892a70af36b498e89ae461e9d9a9525', 'default_handle',
        '444bdaf7e35c4bb08aba35cfe42d7ee5');
INSERT INTO adi_workflow_edge (uuid, workflow_id, source_node_uuid, source_handle, target_node_uuid)
VALUES ('3699e97130634a409e533f685bb291d8', 1, 'e87c362ff13f489dad9568f94e8219ca', '',
        '444bdaf7e35c4bb08aba35cfe42d7ee5');
INSERT INTO adi_workflow_edge (uuid, workflow_id, source_node_uuid, source_handle, target_node_uuid)
VALUES ('2fffb3c242254fe080a965dfe6adfc39', 1, 'f2b59926da754b7a8938eb3f400920ac', '',
        '444bdaf7e35c4bb08aba35cfe42d7ee5');
INSERT INTO adi_workflow_edge (uuid, workflow_id, source_node_uuid, source_handle, target_node_uuid)
VALUES ('bdd6b9530a284e13a083e8aba5bf3b19', 1, '7ac3c57446584c2b8f809daa51091eb0', '',
        '1deb0f2df58c49768a3a5e59f825814d');
INSERT INTO adi_workflow_edge (uuid, workflow_id, source_node_uuid, source_handle, target_node_uuid)
VALUES ('4bec71f5af29429b87f3928a9a20e3d8', 1, '1deb0f2df58c49768a3a5e59f825814d', '',
        '444bdaf7e35c4bb08aba35cfe42d7ee5');
INSERT INTO adi_workflow_edge (uuid, workflow_id, source_node_uuid, source_handle, target_node_uuid)
VALUES ('2e008c4200744f03a4ce0fba352d3d6c', 1, '7a71f5860723457ba5815a71cb21f4d5', 'f325d5e3545a42a184648b88eaaa9f3a',
        '4892a70af36b498e89ae461e9d9a9525');
INSERT INTO adi_workflow_edge (uuid, workflow_id, source_node_uuid, source_handle, target_node_uuid)
VALUES ('ecada0274c38429c99c6bce200d1d132', 1, '7a71f5860723457ba5815a71cb21f4d5', 'caa11c60ac86409a9c0284ef5ea6a284',
        'e87c362ff13f489dad9568f94e8219ca');
INSERT INTO adi_workflow_edge (uuid, workflow_id, source_node_uuid, source_handle, target_node_uuid)
VALUES ('9555c30dcc35410582029b1310321c63', 1, '4892a70af36b498e89ae461e9d9a9525', 'cac3c57446584c2b8f809daa51091ebc',
        '7ac3c57446584c2b8f809daa51091eb0');
INSERT INTO adi_workflow_edge (uuid, workflow_id, source_node_uuid, source_handle, target_node_uuid)
VALUES ('768794ce3d9d4484b2f3d1fcf7cf29ad', 1, '4892a70af36b498e89ae461e9d9a9525', 'ccb59926da754b7a8938eb3f400920ac',
        'f2b59926da754b7a8938eb3f400920ac');

-- mcp server
-- 高德地图, 无需安装，直接使用
-- 此处preset_params中填充了高德的API密钥，preset_params表示的是系统预设的通用配置，用户无需到高德地图的网站获取key即可使用本mcp
insert into adi_mcp (uuid, title, transport_type, sse_url, sse_timeout, install_type, preset_params, website, remark,
                     is_enable)
values (replace(gen_random_uuid()::text, '-', ''), '高德地图', 'sse', 'https://mcp.amap.com/sse', 30, 'remote',
        '[
          {
            "name": "key",
            "title": "高德地图服务API密钥",
            "value": "此处填充您的高德地图API密钥（系统共用）",
            "require_encrypt": true,
            "encrypted": false
          }
        ]', 'https://lbs.amap.com/api/mcp-server/summary',
        '## 产品特点

* 使用简单：适用普通用户基于MCP（SSE）方式，不必部署本地服务，简单通过 URL 地址配置即可使用。
* 自动升级：我们会持续进行迭代更新，无须用户自己任何额外操作使用。
* 更易于大模型理解：我们对原始的JSON结果进行了语义化的转换，更易于大模型理解内容。
* 零运维成本：采用全托管云服务架构，用户无需关心服务器维护、资源扩容等底层运维问题。
* 协议兼容：支持SSE长连接，适配不同业务场景的技术需求。

## 能力介绍

* 生成专属地图
* 导航到目的地
* 打车
* 地理编码
* 逆地理编码
* IP 定位
* 天气查询
* 骑行路径规划
* 步行路径规划
* 驾车路径规划
* 公交路径规划
* 距离测量
* 关键词搜索
* 周边搜索
* 详情搜索',
        true);
-- brave search, 本地使用npx的方式安装
-- 此处customized_param_definitions中指定了用户需要填充的参数（即从Brave search获取的key），表示每个用户在使用本mcp服务前都必须填充该key以便后续MCP进行初始化及调用
insert into adi_mcp (uuid, title, transport_type, stdio_command, stdio_arg, install_type, customized_param_definitions,
                     website,
                     remark,
                     is_enable)
values (replace(gen_random_uuid()::text, '-', ''), 'Brave Search', 'stdio', 'npx',
        '-y @modelcontextprotocol/server-brave-search', 'local',
        '[
          {
            "name": "BRAVE_API_KEY",
            "title": "Brave Search API key",
            "require_encrypt": true
          }
        ]', 'https://github.com/modelcontextprotocol/servers-archived/tree/main/src/brave-search',
        '# Brave Search MCP Server

An MCP server implementation that integrates the Brave Search API, providing both web and local search capabilities.

## Features

- **Web Search**: General queries, news, articles, with pagination and freshness controls
- **Local Search**: Find businesses, restaurants, and services with detailed information
- **Flexible Filtering**: Control result types, safety levels, and content freshness
- **Smart Fallbacks**: Local search automatically falls back to web when no results are found

## Tools

- **brave_web_search**

  - Execute web searches with pagination and filtering
  - Inputs:
    - `query` (string): Search terms
    - `count` (number, optional): Results per page (max 20)
    - `offset` (number, optional): Pagination offset (max 9)

- **brave_local_search**
  - Search for local businesses and services
  - Inputs:
    - `query` (string): Local search terms
    - `count` (number, optional): Number of results (max 20)
  - Automatically falls back to web search if no local results found

## Configuration

### Getting an API Key

1. Sign up for a [Brave Search API account](https://brave.com/search/api/)
2. Choose a plan (Free tier available with 2,000 queries/month)
3. Generate your API key [from the developer dashboard](https://api-dashboard.search.brave.com/app/keys)',
        true);

insert into adi_user_mcp (uuid, user_id, mcp_id, mcp_customized_params, is_enable)
values (replace(gen_random_uuid()::text, '-', ''), 1, 2, '[
  {
    "name": "BRAVE_API_KEY",
    "value": "此处直接填充您的Brave Search API密钥",
    "encrypted": false
  }
]', true);
