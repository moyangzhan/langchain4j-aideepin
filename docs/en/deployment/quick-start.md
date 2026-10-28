# All-in-one Docker Compose Deployment

> [← Deployment](index.md) · [简体中文](../../cn/deployment/quick-start.md)

Bring up every service with one command using the `docker/` directory at the repository root: backend (aideepin-api), Redis, user web, admin web, and nginx. PostgreSQL is not included in the compose stack — install and initialize it first.

## 1. Initialize the Database

### Install PostgreSQL and Extensions

On Ubuntu, add the [PGDG repository](https://www.postgresql.org/download/linux/ubuntu/) first, then install PostgreSQL and the required extensions in one command:

```bash
sudo apt install -y postgresql-16 postgresql-16-pgvector postgresql-16-age
```

> [!NOTE]
> Install extensions as needed: pgvector (vector search) and Apache AGE (knowledge graph) can each be replaced by neo4j — if you use neo4j for one of them, the corresponding extension is not required.

Enable the extensions and create the database:

```sql
CREATE EXTENSION IF NOT EXISTS vector;
CREATE EXTENSION IF NOT EXISTS age;
CREATE DATABASE aideepin;
```

### Run the initialization scripts

The scripts live in `server/db_migration/`; run them in order:

1. `all_ddl.sql` — creates all tables, triggers, and comments
2. `all_dml.sql` — inserts base data (admin account, system config, model platforms, AI models)
3. `all_dml_cn.sql` or `all_dml_en.sql` — pick one, inserts language-specific display data (conversation presets, workflow components, MCP services)

> [!NOTE]
> The default SCHEMA in the DDL is `public`; adjust it if yours differs. Naming conventions and details are in the [db_migration README](https://github.com/moyangzhan/langchain4j-aideepin/blob/main/server/db_migration/README.md).

## 2. Configure and Start

```bash
cd docker

# Edit .env (compose reads .env by default; use .env.prod for production)
# ⚠️ Must update: ADI_DB_HOST, ADI_DB_USERNAME, ADI_DB_PASSWORD,
#    ADI_MAIL_HOST, ADI_MAIL_USERNAME, ADI_MAIL_PASSWORD, ADI_ENCRYPT_AES_KEY

# Servers with ≥ 8G memory: build in parallel directly
docker compose up -d --build

# Servers with < 8G memory: build sequentially to avoid OOM
docker compose build aideepin-admin-web aideepin-user-web
docker compose build aideepin-api
docker compose up -d
```

Common commands: `docker compose logs -f` to follow logs; `docker compose down` to stop all services.

## 3. Architecture

```
Browser → nginx:80
           ├── /api/      → aideepin-api:9999 (backend)
           ├── /admin/    → aideepin-admin-web:80 (admin web)
           └── /          → aideepin-user-web:80 (user web)
```

## 4. First-time Initialization

Enable at least one model platform after startup, or regular users cannot use the app. The admin account is initialized by `all_dml.sql`; after signing in to the admin console:

- **Recommended**: fill in the platform API keys and enable platforms under "Platform Configuration" (see the [Admin Guide](../guide/admin/admin.md));
- or configure directly via SQL (examples):

```sql
-- DeepSeek
UPDATE adi_model_platform SET api_key = 'my_deepseek_secret_key' WHERE name = 'deepseek';

-- OpenAI
UPDATE adi_model_platform SET api_key = 'my_openai_secret_key' WHERE name = 'openai';

-- DashScope
UPDATE adi_model_platform SET api_key = 'my_dashscope_api_key' WHERE name = 'dashscope';

-- SiliconFlow
UPDATE adi_model_platform SET api_key = 'my_siliconflow_api_key' WHERE name = 'siliconflow_setting';

-- Ollama
UPDATE adi_model_platform SET base_url = 'my_ollama_base_url' WHERE name = 'ollama';
```

Then enable the models you need:

```sql
UPDATE adi_ai_model SET is_enable = true WHERE name = 'deepseek-v4-flash';
```

## Configuration Reference (.env)

| Variable | Description | Default |
|----------|-------------|---------|
| TZ | Timezone | Asia/Shanghai |
| IMAGE_VERSION | Backend image version | v1.1.0-dev |
| APP_VERSION | Application version | 1.0.0-SNAPSHOT |
| APP_PROFILE | Spring profile (dev/prod) | dev |
| JAVA_OPTS | JVM options | -Xms1024m -Xmx2048m ... |
| ADI_DB_HOST | PostgreSQL host | localhost |
| ADI_DB_PORT | PostgreSQL port | 5432 |
| ADI_DB_NAME | PostgreSQL database name | aideepin |
| ADI_DB_USERNAME | PostgreSQL user | your-db-account |
| ADI_DB_PASSWORD | PostgreSQL password | your-db-password |
| ADI_REDIS_HOST | Redis host | redis |
| ADI_REDIS_PORT | Redis port | 6379 |
| ADI_MAIL_HOST | SMTP mail host | your-email-host |
| ADI_MAIL_USERNAME | SMTP mail username | your-email-username |
| ADI_MAIL_PASSWORD | SMTP mail password | your-email-password |
| ADI_ENCRYPT_AES_KEY | AES encryption key (16 chars) | your-16-char-key |
| ADI_NEO4J_HOST | Neo4j host (optional) | localhost |
| ADI_NEO4J_PORT | Neo4j port (optional) | 7687 |
| ADI_NEO4J_USERNAME | Neo4j username (optional) | neo4j |
| ADI_NEO4J_PASSWORD | Neo4j password (optional) | your-neo4j-password |
| ADI_NEO4J_DATABASE | Neo4j database (optional) | neo4j |

> [!TIP]
> For production, set `APP_PROFILE` to `prod` and manage configuration via `.env.prod`.

---

Previous: [Deployment](index.md) · Next: [Per-service Docker deployment](docker-split.md)
