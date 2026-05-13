> **[🇨🇳 中文文档](README.zh-CN.md)** | English

## Backend - Docker Deployment

### Usage

```bash
# Option 1: Edit .env and then start (uses .env by default; for production use .env.prod)
# ⚠️ Must update: ADI_DB_HOST, ADI_DB_USERNAME, ADI_DB_PASSWORD, ADI_MAIL_HOST, ADI_MAIL_USERNAME, ADI_MAIL_PASSWORD, ADI_ENCRYPT_AES_KEY
docker-compose up -d --build
# docker-compose --env-file .env.prod up -d --build

# Option 2: Pass environment variables via command line (overrides --env-file)
TZ=America/New_York ADI_DB_HOST=192.168.1.100 ADI_DB_PASSWORD=mypassword ADI_MAIL_HOST=smtp.example.com ADI_MAIL_USERNAME=user@example.com ADI_MAIL_PASSWORD=mypassword ADI_ENCRYPT_AES_KEY=PLEASE_REPLACE_ME docker-compose --env-file .env.prod up -d --build

# View status
docker ps

# Stop
docker-compose down
```

### Notes

- Starts the backend service and Redis
- Backend port: 9999
- Configure `.env` or `.env.prod` before starting
- **PostgreSQL must be installed manually**, with pgvector (vector search) and/or Apache AGE (graph search) extensions as needed. See [db_migration/README](../db_migration/README.md)
- If you want to run PostgreSQL in Docker, these images are available:
  - [pgvector/pgvector](https://hub.docker.com/r/pgvector/pgvector) — PostgreSQL + pgvector
  - [apache/age](https://hub.docker.com/r/apache/age) — PostgreSQL + Apache AGE
  - [petescarth/postgres-datascience](https://hub.docker.com/r/petescarth/postgres-datascience) — PostgreSQL + pgvector + Apache AGE (community maintained)
- This configuration only starts backend services. To start all services (including frontends) at once, use [docker/](../../docker/README.md) in the root directory
