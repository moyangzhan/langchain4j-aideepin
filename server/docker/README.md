> **[🇨🇳 中文文档](README.zh-CN.md)** | English

## Backend - Docker Deployment

### Usage

```bash
# Build JAR first
cd ..
mvn clean package -Dmaven.test.skip=true

# Then start with Docker
cd docker
docker-compose up -d --build

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
