## Getting Started

> 中文文档 | **[🇬🇧 English](README.md)**

**LangChain4j-AIDeepin（得应AI） 是基于 AI 的工作效率提升工具。**

*可用于辅助企业/团队进行技术研发、产品设计、人事/财务/IT信息咨询、系统/商品咨询、客服话术支撑等工作*

## 系统组成及文档

```
AIDEEPIN
  |__ 服务端 (langchain4j-aideepin)
  |__ 用户端 WEB (langchain4j-aideepin-web)
  |__ 管理端 WEB (langchain4j-aideepin-admin)
```

👉 [详细文档](https://github.com/moyangzhan/langchain4j-aideepin/wiki)

后端服务代码地址：[github](https://github.com/moyangzhan/langchain4j-aideepin) | [gitee](https://gitee.com/moyangzhan/langchain4j-aideepin)

前端项目：

- 用户端 WEB：langchain4j-aideepin-web
  - [github](https://github.com/moyangzhan/langchain4j-aideepin-web) | [gitee](https://gitee.com/moyangzhan/langchain4j-aideepin-web)
- 管理端 WEB：langchain4j-aideepin-admin
  - [github](https://github.com/moyangzhan/langchain4j-aideepin-admin) | [gitee](https://gitee.com/moyangzhan/langchain4j-aideepin-admin)

## 体验网址

[http://www.aideepin.com](http://www.aideepin.com/)

## 功能点

### AI 对话

多会话（多角色）支持，每个会话可配置系统提示词、模型和参数，支持流式输出。

### 图片生成

支持 GPT-Image-2、灵积万相等模型，提供文生图、图片编辑、图片变体、背景生成等功能。

### 知识库（RAG）

上传文档（PDF、Word、PPT、Excel 等）构建知识库，通过检索增强生成提升 AI 回复质量。

- **向量搜索**：使用 `all-minilm-l6-v2`、`bge-small-zh-v1.5` 等模型进行文档向量化，通过相似度搜索检索相关内容
- **图搜索**：从文档中提取实体和关系构建知识图谱（Apache AGE 或 Neo4j），实现结构化知识检索

### AI 工作流

可视化工作流编辑器，支持条件分支、并行执行，以及 LLM 调用、知识库查询、代码执行、人工反馈等多种节点类型。

### MCP 服务市场

发现和集成 MCP（Model Context Protocol）服务，扩展 AI 的外部工具和数据源能力，支持 SSE 和 stdio 传输类型。

### ASR & TTS

支持灵活的语音输入输出组合：

- 文字提问 → 文字回复
- 文字提问 → 语音回复
- 语音提问 → 文字回复
- 语音提问 → 语音回复

AI 音色可选。

### 长期记忆

自动从对话中提取和存储关键信息作为用户记忆，使 AI 能够基于历史上下文进行个性化回复。

### 存储

- 本地存储
- 阿里云 OSS

## 已集成的模型平台功能

| 模型平台   | 对话 | 文生图 | 背景生成 | 图像识别 | 语音合成 | 语音识别 |
|:---------|:----:|:-----:|:-------:|:-------:|:-------:|:-------:|
| OpenAI   |  ✓   |   ✓   |         |         |         |         |
| 灵积      |  ✓   |   ✓   |    ✓    |    ✓    |    ✓    |    ✓    |
| 硅基流动   |  ✓   |   ✓   |         |    ✓    |    ✓    |    ✓    |
| Ollama   |  ✓   |       |         |         |         |         |
| DeepSeek |  ✓   |       |         |         |         |         |

## 技术栈

该仓库为后端服务。

后端：

- JDK 17
- Spring Boot 3.5.14
- [langchain4j](https://github.com/langchain4j/langchain4j)（Java 版 LangChain）
- [langgraph4j](https://github.com/bsorrentino/langgraph4j)
- PostgreSQL
  - [pgvector](https://github.com/pgvector/pgvector) 扩展（向量数据库）
  - [Apache AGE](https://github.com/apache/age) 扩展（图数据库）
- [neo4j](https://neo4j.com/deployment-center/)（pgvector + Apache AGE 的替代方案）

前端：

- Vue 3
- Vite
- TypeScript
- pnpm
- Pinia
- Naive UI

## 如何部署

### 初始化

**a. 初始化数据库**

1. 创建数据库 `aideepin`
2. 执行 `db_migration/all_ddl.sql` 创建表结构
3. 执行 `db_migration/all_dml.sql` 插入基础数据
4. 执行 `db_migration/all_dml_cn.sql`（中文）或 `db_migration/all_dml_en.sql`（英文）插入展示数据
5. 配置并启用模型平台（至少启用一个，可参考上方表格选择），或使用[管理端](https://github.com/moyangzhan/langchain4j-aideepin-admin)在界面上配置

配置模型平台：

```sql
-- DeepSeek
UPDATE adi_model_platform SET api_key = 'my_deepseek_secret_key' WHERE name = 'deepseek';

-- OpenAI
UPDATE adi_model_platform SET api_key = 'my_openai_secret_key' WHERE name = 'openai';

-- 灵积
UPDATE adi_model_platform SET api_key = 'my_dashcope_api_key' WHERE name = 'dashscope';

-- 硅基流动
UPDATE adi_model_platform SET api_key = 'my_siliconflow_api_key' WHERE name = 'siliconflow_setting';

-- Ollama
UPDATE adi_model_platform SET base_url = 'my_ollama_base_url' WHERE name = 'ollama';
```

启用模型或新增模型：

```sql
-- 启用模型
UPDATE adi_ai_model SET is_enable = true WHERE name = 'deepseek-v4-flash';
UPDATE adi_ai_model SET is_enable = true WHERE name = 'gpt-3.5-turbo';
UPDATE adi_ai_model SET is_enable = true WHERE name = 'gpt-image-2';
UPDATE adi_ai_model SET is_enable = true WHERE name = 'qwen-turbo';
UPDATE adi_ai_model SET is_enable = true WHERE name = 'THUDM/GLM-Z1-9B-0414';
UPDATE adi_ai_model SET is_enable = true WHERE name = 'tinydolphin';

-- 新增模型
INSERT INTO adi_ai_model (name, type, platform, is_enable) VALUES ('vicuna', 'text', 'ollama', true);
```

配置搜索引擎（Google）：

```sql
UPDATE adi_sys_config SET value = '{"url":"https://www.googleapis.com/customsearch/v1","key":"my key from cloud.google.com","cx":"my cx from programmablesearchengine.google.com"}' WHERE name = 'google_setting';
```

**b. 修改配置文件**

复制示例配置并重命名：

```bash
cp adi-bootstrap/src/main/resources/application-dev.yml.example adi-bootstrap/src/main/resources/application-dev.yml
```

然后修改以下配置项：

- PostgreSQL：`application-[dev|prod].yml` → `spring.datasource`
- Redis：`application-[dev|prod].yml` → `spring.data.redis`
- 邮箱：`application.yml` → `spring.mail`
- 向量数据库（默认 pgvector）：`application-[dev|prod].yml` → `adi.vector-database=[pgvector|neo4j]`
- 图数据库（默认 Apache AGE）：`application-[dev|prod].yml` → `adi.graph-database=[apache-age|neo4j]`

### 编译及运行

```bash
cd langchain4j-aideepin
mvn clean package -Dmaven.test.skip=true
```

JAR 包启动：

```bash
cd adi-bootstrap/target
nohup java -jar -Xms768m -Xmx1024m -XX:+HeapDumpOnOutOfMemoryError \
  adi-bootstrap-0.0.1-SNAPSHOT.jar --spring.profiles.active=[dev|prod] \
  /dev/null 2>&1 &
```

Docker 启动：

```bash
cd adi-bootstrap
docker build . -t aideepin:0.0.1
docker run -d \
  --name=aideepin \
  -p 8888:9999 \
  -e APP_PROFILE=[dev|prod] \
  -v="/data/aideepin/logs:/data/logs" \
  aideepin:0.0.1
```

## 贡献指南

欢迎任何形式的贡献，包括但不限于：

- 提交 Bug 报告
- 提出功能建议
- 改进文档
- 提交代码（PR）

代码提交流程：

1. Fork 本仓库
2. 创建特性分支（`git checkout -b feature/xxx`）
3. 提交更改（`git commit -m 'feat: xxx'`）
4. 推送分支（`git push origin feature/xxx`）
5. 提交 Pull Request

## 截图

**AI 对话：**

![AI聊天](image/README/1691583184761.png)

**AI 画图：**

![AI画图](image/README/draw_001.png "AI绘图")

**知识库：**

![知识库](image/README/kbidx.png)

**向量化：**

![向量化](image/README/kb03.png)

**知识图谱：**

![知识图谱](image/README/kb_graph_01.png)

**工作流：**

![工作流](image/README/workflow.png)

## ⭐ 支持项目

如果 LangChain4j-AIDeepin 对您有帮助，欢迎：

- 给仓库点个 Star
- 推荐给身边的人
- 分享您的使用体验

## ❤️ 感谢

**Contributors**

<a href="https://github.com/moyangzhan/langchain4j-aideepin/graphs/contributors">
  <img src="https://contrib.rocks/image?repo=moyangzhan/langchain4j-aideepin" />
</a>

<br/>

## 推荐项目

[Mango Finder](https://github.com/moyangzhan/mango-finder) — 一款用自然语言搜索本地文件的桌面应用，支持文档、图片、音频的语义搜索及跨设备搜索功能。帮助您根据记忆中的内容查找信息，而不需要记住文件名或文件夹结构。
