> 中文文档 | **[🇬🇧 English](README.md)**

## 后端服务 - Docker 部署

### 使用

```bash
# 方式一：修改 .env 后直接启动（默认读取 .env；如需生产配置可指定 .env.prod）
# ⚠️ 必须修改：ADI_DB_HOST、ADI_DB_USERNAME、ADI_DB_PASSWORD、ADI_MAIL_HOST、ADI_MAIL_USERNAME、ADI_MAIL_PASSWORD、ADI_ENCRYPT_AES_KEY
docker-compose up -d --build
# docker-compose --env-file .env.prod up -d --build

# 方式二：通过命令行传入环境变量（优先级高于 --env-file）
TZ=Asia/Shanghai ADI_DB_HOST=192.168.1.100 ADI_DB_PASSWORD=mypassword ADI_MAIL_HOST=smtp.example.com ADI_MAIL_USERNAME=user@example.com ADI_MAIL_PASSWORD=mypassword ADI_ENCRYPT_AES_KEY=PLEASE_REPLACE_ME docker-compose --env-file .env.prod up -d --build

# 查看运行状态
docker ps

# 停止
docker-compose down
```

### 说明

- 启动后端服务和 Redis
- 后端服务端口：9999
- 启动前需先配置 `.env` 或 `.env.prod` 文件
- **PostgreSQL 需要自行安装**，并按需安装 pgvector（向量搜索）和/或 Apache AGE（图搜索）扩展，详见 [db_migration/README](../db_migration/README.zh-CN.md)
- 如果想用 Docker 运行 PostgreSQL，可参考以下镜像：
  - [pgvector/pgvector](https://hub.docker.com/r/pgvector/pgvector) — PostgreSQL + pgvector
  - [apache/age](https://hub.docker.com/r/apache/age) — PostgreSQL + Apache AGE
  - [petescarth/postgres-datascience](https://hub.docker.com/r/petescarth/postgres-datascience) — PostgreSQL + pgvector + Apache AGE（社区维护）
- 本配置仅启动后端相关服务，如需一键启动全部服务（含前端），请使用根目录的 [docker/](../../docker/README.zh-CN.md)
