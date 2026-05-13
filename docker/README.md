## Quick Start with Docker Compose

> **[🇨🇳 中文文档](README.zh-CN.md)** | English

Start all services (backend, Redis, user web, admin web, nginx) with a single command.

### Prerequisites

- Docker & Docker Compose installed
- PostgreSQL database initialized (see [server/README](../server/README.md))

### Usage

```bash
cd docker

# Build and start all services
docker-compose up -d --build

# View logs
docker-compose logs -f

# Stop all services
docker-compose down
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

### Standalone Deployment

If you need to deploy services to different servers (e.g., admin web on intranet, user web on public network), each sub-project has its own docker-compose:

- `server/docker/` — Backend + Redis
- `user-web/docker/` — User web + Nginx
- `admin-web/docker/` — Admin web + Nginx

For example, to deploy a sub-project independently:

```bash
cd <sub-project>/docker-compose
docker-compose up -d --build
```
