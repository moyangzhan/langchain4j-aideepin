# Bare-metal Deployment

> [← Deployment](index.md) · [简体中文](../../cn/deployment/bare-metal.md)

For environments without Docker: run the backend as a JAR directly and host the web build outputs yourself.

## Build

```bash
cd server
mvn clean package -Dmaven.test.skip=true
```

## Configuration

Create `application-prod.yml` modeled on `adi-bootstrap/src/main/resources/application-dev.yml.example` (same set of options), and update:

- PostgreSQL: `spring.datasource`
- Redis: `spring.data.redis`
- Mail: `application.yml` → `spring.mail`
- Vector database (pgvector by default): `adi.vector-database=[pgvector|neo4j]`
- Graph database (Apache AGE by default): `adi.graph-database=[apache-age|neo4j]`

## Start the backend

```bash
cd adi-bootstrap/target
nohup java -jar -Xms768m -Xmx1024m -XX:+HeapDumpOnOutOfMemoryError \
  adi-bootstrap-0.0.1-SNAPSHOT.jar --spring.profiles.active=prod \
  > /dev/null 2>&1 &
```

## Deploy the frontends

Build each web app inside `user-web/` and `admin-web/`:

```bash
pnpm build
```

Host the `dist/` outputs with your own nginx (or any static file server) and reverse-proxy `/api/` to the backend on port 9999 (stripping the `/api` prefix). Reference nginx configs: `docker/nginx/nginx.conf` and `user-web/README.md`.

---

Previous: [Per-service Docker deployment](docker-split.md) · Next: [Upgrade](upgrade.md)
