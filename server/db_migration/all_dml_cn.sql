-- 预设角色
INSERT INTO adi_conversation_preset (uuid, title, remark, ai_system_message)
VALUES ('26a8f54c560948d6b2d4969f08f3f2fb', '开发工程师', '技术好', '你是一个经验丰富的开发工程师,开发技能极其熟练');
INSERT INTO adi_conversation_preset (uuid, title, remark, ai_system_message)
VALUES ('16a8f54c560949d6b2d4969f08f3f2fc', '财务专家', '算数很厉害,相关法律知识也很了解',
        '你是一个经验丰富的财务专家,精通财务分析、预算编制、财务报告、税务法规等领域知识');

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
