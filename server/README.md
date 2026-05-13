## Getting Started

> **[🇨🇳 中文文档](README.zh-CN.md)** | English

This directory contains the backend service. For project overview, see the [root README](../README.md).

## Tech Stack

- JDK 17
- Spring Boot 3.5.14
- [langchain4j](https://github.com/langchain4j/langchain4j) (Java version of LangChain)
- [langgraph4j](https://github.com/bsorrentino/langgraph4j)
- PostgreSQL
  - [pgvector](https://github.com/pgvector/pgvector) extension (vector database)
  - [Apache AGE](https://github.com/apache/age) extension (graph database)
- [neo4j](https://neo4j.com/deployment-center/) (alternative to pgvector + Apache AGE)

## How to Deploy

### Initialization

**a. Initialize the database**

1. Create the database `aideepin`
2. Execute `db_migration/all_ddl.sql` to create tables
3. Execute `db_migration/all_dml.sql` to insert base data
4. Execute `db_migration/all_dml_en.sql` (English) or `db_migration/all_dml_cn.sql` (Chinese) to insert display data
5. Enable and configure the model platform (also referred to as model provider) or use the [admin web](../admin-web/README.md) to configure via the interface

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

Configure search engine (required for workflow Google search node):

```sql
UPDATE adi_sys_config SET value = '{"url":"https://www.googleapis.com/customsearch/v1","key":"my_google_api_key","cx":"my_cx"}' WHERE name = 'google_setting';
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
- Vector database (default pgvector): `application-[dev|prod].yml` → `adi.vector-database=[pgvector|neo4j]`
- Graph database (default Apache AGE): `application-[dev|prod].yml` → `adi.graph-database=[apache-age|neo4j]`

### Build and Run

```bash
cd server
mvn clean package -Dmaven.test.skip=true
```

Start with JAR:

```bash
cd adi-bootstrap/target
nohup java -jar -Xms768m -Xmx1024m -XX:+HeapDumpOnOutOfMemoryError \
  adi-bootstrap-0.0.1-SNAPSHOT.jar --spring.profiles.active=[dev|prod] \
  /dev/null 2>&1 &
```

Start with Docker (backend only, for all services use [docker/](../docker/README.md)):

```bash
cd server
docker build . -t aideepin-api:0.0.1
docker run -d \
  --name=aideepin-api \
  -p 8888:9999 \
  -e APP_PROFILE=[dev|prod] \
  -v="/data/aideepin/logs:/data/logs" \
  aideepin-api:0.0.1
```
