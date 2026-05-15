## Quick Start with Docker Compose

> **[🇨🇳 中文文档](README.zh-CN.md)** | English

Start all services (backend, Redis, user web, admin web, nginx) with a single command.

### Prerequisites

- Docker & Docker Compose installed
- PostgreSQL database initialized (see [server/README](../server/README.md))

### Usage

```bash
# Run from project root
cd docker

# Option 1: Edit .env and then start (uses .env by default; for production use .env.prod)
# ⚠️ Must update: ADI_DB_HOST, ADI_DB_USERNAME, ADI_DB_PASSWORD, ADI_MAIL_HOST, ADI_MAIL_USERNAME, ADI_MAIL_PASSWORD, ADI_ENCRYPT_AES_KEY

# Servers with ≥ 8GB RAM (parallel build):
# docker compose up -d --build

# Servers with < 8GB RAM (sequential build to avoid OOM):
docker compose build aideepin-admin-web aideepin-user-web
docker compose build aideepin-api
docker compose up -d

# Option 2: Pass environment variables via command line (overrides --env-file)
# Servers with ≥ 8GB RAM:
TZ=America/New_York ADI_DB_HOST=192.168.1.100 ADI_DB_PASSWORD=mypassword ADI_MAIL_HOST=smtp.example.com ADI_MAIL_USERNAME=user@example.com ADI_MAIL_PASSWORD=mypassword ADI_ENCRYPT_AES_KEY=PLEASE_REPLACE_ME docker compose --env-file .env.prod up -d --build

# Servers with < 8GB RAM:
docker compose --env-file .env.prod build aideepin-admin-web aideepin-user-web
docker compose --env-file .env.prod build aideepin-api
TZ=America/New_York ADI_DB_PASSWORD=mypassword ADI_DB_USERNAME=postgres ADI_MAIL_PASSWORD=mypassword ADI_ENCRYPT_AES_KEY=PLEASE_REPLACE_ME docker compose --env-file .env.prod up -d

# View logs
docker compose logs -f

# Stop all services
docker compose down
```

### Architecture

```
Browser → nginx:80
            ├── /api/      → aideepin-api:9999 (backend)
            ├── /admin/    → aideepin-admin-web:80 (admin frontend)
            └── /          → aideepin-user-web:80 (user frontend)
```

### Configuration

Edit `.env` to configure:

| Variable | Description | Default |
|----------|-------------|---------|
| TZ | Timezone | Asia/Shanghai |
| IMAGE_VERSION | Backend image tag | v1.1.0-dev |
| APP_VERSION | App version | 1.0.0-SNAPSHOT |
| APP_PROFILE | Spring profile (dev/prod) | dev |
| JAVA_OPTS | JVM options | -Xms1024m -Xmx2048m ... |
| ADI_DB_HOST | PostgreSQL host | localhost |
| ADI_DB_PORT | PostgreSQL port | 5432 |
| ADI_DB_NAME | PostgreSQL database name | aideepin |
| ADI_DB_USERNAME | PostgreSQL username | your-db-account |
| ADI_DB_PASSWORD | PostgreSQL password | your-db-password |
| ADI_REDIS_HOST | Redis host | redis |
| ADI_REDIS_PORT | Redis port | 6379 |
| ADI_MAIL_HOST | SMTP mail host | your-email-host |
| ADI_MAIL_USERNAME | SMTP mail username | your-email-username |
| ADI_MAIL_PASSWORD | SMTP mail password | your-email-password |
| ADI_ENCRYPT_AES_KEY | AES encryption key (16-char string) | your-16-char-key |
| ADI_NEO4J_HOST | Neo4j host (optional) | localhost |
| ADI_NEO4J_PORT | Neo4j port (optional) | 7687 |
| ADI_NEO4J_USERNAME | Neo4j username (optional) | neo4j |
| ADI_NEO4J_PASSWORD | Neo4j password (optional) | your-neo4j-password |
| ADI_NEO4J_DATABASE | Neo4j database name (optional) | neo4j |

### Standalone Deployment

If you need to deploy services to different servers (e.g., admin web on intranet, user web on public network), each sub-project has its own docker compose:

- `server/docker/` — Backend + Redis
- `user-web/docker/` — User web + Nginx
- `admin-web/docker/` — Admin web + Nginx

For example, to deploy a sub-project independently:

```bash
cd <sub-project>/docker
docker compose --env-file .env.prod up -d --build
```
