# Docker 独立部署

> [← 部署](index.md) · [English](../../en/deployment/docker-split.md)

一键部署把所有服务放在同一套 compose 里。需要拆开部署时（例如管理端放内网、用户端放公网），使用各子项目自带的 compose 配置独立部署：

| 目录 | 内容 |
|------|------|
| `server/docker/` | 后端服务 + Redis |
| `user-web/docker/` | 用户端前端 + Nginx |
| `admin-web/docker/` | 管理端前端 + Nginx |

分别在对应目录执行：

```bash
cd <子项目>/docker
docker compose --env-file .env.prod up -d --build
```

数据库初始化与[一键部署](quick-start.md#1-初始化数据库)相同。

---

上一篇：[Docker Compose 一键部署](quick-start.md) ｜ 下一篇：[裸机部署](bare-metal.md)
