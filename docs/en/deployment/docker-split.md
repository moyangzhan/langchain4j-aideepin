# Per-service Docker Deployment

> [← Deployment](index.md) · [简体中文](../../cn/deployment/docker-split.md)

The all-in-one stack puts every service in a single compose file. When you need to split them (e.g. admin web on an intranet, user web on a public network), deploy each sub-project with its own compose config:

| Directory | Contents |
|-----------|----------|
| `server/docker/` | Backend service + Redis |
| `user-web/docker/` | User web + nginx |
| `admin-web/docker/` | Admin web + nginx |

Run in each directory:

```bash
cd <sub-project>/docker
docker compose --env-file .env.prod up -d --build
```

Database initialization is the same as in the [all-in-one deployment](quick-start.md#1-initialize-the-database).

---

Previous: [All-in-one Docker Compose deployment](quick-start.md) · Next: [Bare-metal deployment](bare-metal.md)
