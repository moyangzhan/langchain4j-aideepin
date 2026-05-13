> 中文文档 | **[🇬🇧 English](README.md)**

## 后端服务 - Docker 部署

### 使用

```bash
# 先编译打包
cd ..
mvn clean package -Dmaven.test.skip=true

# 再用 Docker 启动
cd docker
docker-compose up -d --build

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
