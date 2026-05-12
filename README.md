## Getting Started

> **[🇨🇳 中文文档](README.zh-CN.md)** | English

**LangChain4j-AIDeepin is an AI-based productivity enhancement tool.**

*It can be used to assist enterprises/teams in technical research and development, product design, HR/finance/IT information consulting, system/product consulting, customer service support, etc.*

## System Composition and Documentation

```
AIDEEPIN
  |__ Server (langchain4j-aideepin)
  |__ User Web (langchain4j-aideepin-web)
  |__ Admin Web (langchain4j-aideepin-admin)
```

👉 [Detailed Documentation](docs/en/index.md)

Backend source repository: [github](https://github.com/moyangzhan/langchain4j-aideepin)

Frontend projects:

- User Web: langchain4j-aideepin-web
  - [github](https://github.com/moyangzhan/langchain4j-aideepin-web)
- Admin Web: langchain4j-aideepin-admin
  - [github](https://github.com/moyangzhan/langchain4j-aideepin-admin)

## Demo URL

[http://www.aideepin.com](http://www.aideepin.com/)

## Features

### AI Chat

Multi-conversation support with different AI roles. Each conversation can be configured with a specific system prompt, model, and parameters. Supports streaming output with real-time token display.

### Image Generation

Generate images from text prompts using models like GPT-Image-2 and DashScope's Wanx. Supports text-to-image, image editing, image variation, and background generation.

### Knowledge Base (RAG)

Build knowledge bases from uploaded documents (PDF, Word, PPT, Excel, etc.) and use them to enhance AI responses with retrieval-augmented generation.

- **Vector Search**: Embed documents into vector space using models like `all-minilm-l6-v2` or `bge-small-zh-v1.5`, then retrieve relevant chunks via similarity search
- **Graph Search**: Extract entities and relationships from documents to build knowledge graphs using Apache AGE or Neo4j, enabling structured knowledge retrieval

### AI Workflow

Visual workflow editor for building complex AI pipelines. Supports conditional branching, parallel execution, and various node types including LLM calls, knowledge base queries, code execution, and human feedback loops.

### MCP Service Marketplace

Discover and integrate MCP (Model Context Protocol) servers to extend AI capabilities with external tools and data sources. Supports SSE and stdio transport types.

### ASR & TTS

Full voice interaction support with flexible input/output combinations:

- Text question → Text response
- Text question → Voice response
- Voice question → Text response
- Voice question → Voice response

### Long-term Memory

Automatically extracts and stores key information from conversations as user memories, allowing the AI to personalize responses based on historical context.

### Storage

- Local file storage
- Alibaba Cloud OSS integration

## Integrated Platform Features

| Model Platform | Chat | Image Generation | Background Generation | Image Recognition | Text-to-Speech | Speech Recognition |
|:---------------|:----:|:----------------:|:---------------------:|:-----------------:|:--------------:|:------------------:|
| OpenAI         |  ✓   |        ✓         |                       |                   |                |                    |
| Dashscope      |  ✓   |        ✓         |           ✓           |         ✓         |       ✓        |         ✓          |
| SiliconFlow    |  ✓   |        ✓         |                       |         ✓         |       ✓        |         ✓          |
| Ollama         |  ✓   |                  |                       |                   |                |                    |
| DeepSeek       |  ✓   |                  |                       |                   |                |                    |

## Tech Stack

This repository is for the backend service.

Backend:

- JDK 17
- Spring Boot 3.5.14
- [langchain4j](https://github.com/langchain4j/langchain4j) (Java version of LangChain)
- [langgraph4j](https://github.com/bsorrentino/langgraph4j)
- PostgreSQL
  - [pgvector](https://github.com/pgvector/pgvector) extension (vector database)
  - [Apache AGE](https://github.com/apache/age) extension (graph database)
- [neo4j](https://neo4j.com/deployment-center/) (alternative to pgvector + Apache AGE)

Frontend:

- Vue 3
- Vite
- TypeScript
- pnpm
- Pinia
- Naive UI

## How to Deploy

### Initialization

**a. Initialize the database**

1. Create the database `aideepin`
2. Execute `db_migration/all_ddl.sql` to create tables
3. Execute `db_migration/all_dml.sql` to insert base data
4. Execute `db_migration/all_dml_en.sql` (English) or `db_migration/all_dml_cn.sql` (Chinese) to insert display data
5. Enable and configure the model platform (also referred to as model provider) or use the [admin web](https://github.com/moyangzhan/langchain4j-aideepin-admin) to configure via the interface

Configure model platforms:

```sql
-- DeepSeek
UPDATE adi_model_platform SET api_key = 'my_deepseek_secret_key' WHERE name = 'deepseek';

-- OpenAI
UPDATE adi_model_platform SET api_key = 'my_openai_secret_key' WHERE name = 'openai';

-- Dashscope
UPDATE adi_model_platform SET api_key = 'my_dashcope_api_key' WHERE name = 'dashscope';

-- Siliconflow
UPDATE adi_model_platform SET api_key = 'my_siliconflow_api_key' WHERE name = 'siliconflow_setting';

-- Ollama
UPDATE adi_model_platform SET base_url = 'my_ollama_base_url' WHERE name = 'ollama';
```

Enable models or add new models:

```sql
-- Enable models
UPDATE adi_ai_model SET is_enable = true WHERE name = 'deepseek-v4-flash';
UPDATE adi_ai_model SET is_enable = true WHERE name = 'gpt-3.5-turbo';
UPDATE adi_ai_model SET is_enable = true WHERE name = 'gpt-image-2';
UPDATE adi_ai_model SET is_enable = true WHERE name = 'qwen-turbo';
UPDATE adi_ai_model SET is_enable = true WHERE name = 'THUDM/GLM-Z1-9B-0414';
UPDATE adi_ai_model SET is_enable = true WHERE name = 'tinydolphin';

-- Add new model
INSERT INTO adi_ai_model (name, type, platform, is_enable) VALUES ('vicuna', 'text', 'ollama', true);
```

Configure search engine (Google):

```sql
UPDATE adi_sys_config SET value = '{"url":"https://www.googleapis.com/customsearch/v1","key":"my key from cloud.google.com","cx":"my cx from programmablesearchengine.google.com"}' WHERE name = 'google_setting';
```

**b. Modify the configuration file**

Copy the example config and rename it:

```bash
cp adi-bootstrap/src/main/resources/application-dev.yml.example adi-bootstrap/src/main/resources/application-dev.yml
```

Then modify the following entries:

- PostgreSQL: `application-[dev|prod].yml` → `spring.datasource`
- Redis: `application-[dev|prod].yml` → `spring.data.redis`
- Mail: `application.yml` → `spring.mail`

### Build and Run

```bash
cd langchain4j-aideepin
mvn clean package -Dmaven.test.skip=true
```

Start with JAR:

```bash
cd adi-bootstrap/target
nohup java -jar -Xms768m -Xmx1024m -XX:+HeapDumpOnOutOfMemoryError \
  adi-bootstrap-0.0.1-SNAPSHOT.jar --spring.profiles.active=[dev|prod] \
  /dev/null 2>&1 &
```

Start with Docker:

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

## Contributing Guidelines

All forms of contributions are welcome, including but not limited to:

- Submitting Bug Reports
- Proposing New Features
- Improving Documentation
- Submitting Code (PR)

Code Submission Process:

1. Fork this repository
2. Create a feature branch (`git checkout -b feature/xxx`)
3. Commit changes (`git commit -m 'feat: xxx'`)
4. Push the branch (`git push origin feature/xxx`)
5. Submit a Pull Request

## ⭐ Support the Project

If you find LangChain4j-AIDeepin useful, please consider:

- Starring the repository on GitHub
- Recommending it to others
- Sharing your experience

## ❤️ Thanks to

**Contributors**

<a href="https://github.com/moyangzhan/langchain4j-aideepin/graphs/contributors">
  <img src="https://contrib.rocks/image?repo=moyangzhan/langchain4j-aideepin" />
</a>

<br/>

## Recommended Projects

[Mango Finder](https://github.com/moyangzhan/mango-finder) — A local-first desktop app for semantic search across documents, images, and audio files using natural language, with cross-device search support. It helps you find information based on what you remember, not file names or folder structures.
