## Docker Compose 快速启动

> 中文文档 | **[🇬🇧 English](README.md)**

一条命令启动所有服务（后端、Redis、用户端、管理端、Nginx）。

### 前置条件

- 已安装 Docker 和 Docker Compose
- 已安装并初始化 PostgreSQL 数据库（见 [server/README](../server/README.zh-CN.md)）

### 使用

```bash
# 在项目根目录下执行
cd docker

# 方式一：修改 .env 后直接启动（默认读取 .env；如需生产配置可指定 .env.prod）
# ⚠️ 必须修改：ADI_DB_HOST、ADI_DB_USERNAME、ADI_DB_PASSWORD、ADI_MAIL_HOST、ADI_MAIL_USERNAME、ADI_MAIL_PASSWORD、ADI_ENCRYPT_AES_KEY

# 内存 ≥ 8G 的服务器，可直接并行构建：
# docker compose up -d --build

# 内存 < 8G 的服务器，建议顺序构建以避免 OOM：
docker compose build aideepin-admin-web aideepin-user-web
docker compose build aideepin-api
docker compose up -d

# 方式二：通过命令行传入环境变量（优先级高于 --env-file）
# 内存 ≥ 8G 的服务器：
TZ=Asia/Shanghai ADI_DB_HOST=192.168.1.100 ADI_DB_PASSWORD=mypassword ADI_MAIL_HOST=smtp.example.com ADI_MAIL_USERNAME=user@example.com ADI_MAIL_PASSWORD=mypassword ADI_ENCRYPT_AES_KEY=PLEASE_REPLACE_ME docker compose --env-file .env.prod up -d --build

# 内存 < 8G 的服务器：
docker compose --env-file .env.prod build aideepin-admin-web aideepin-user-web
docker compose --env-file .env.prod build aideepin-api
TZ=Asia/Shanghai ADI_DB_PASSWORD=mypassword ADI_DB_USERNAME=postgres ADI_MAIL_PASSWORD=mypassword ADI_ENCRYPT_AES_KEY=PLEASE_REPLACE_ME docker compose --env-file .env.prod up -d

# 查看日志
docker compose logs -f

# 停止所有服务
docker compose down
```

### 架构

```
浏览器 → nginx:80
          ├── /api/      → aideepin-api:9999（后端服务）
          ├── /admin/    → aideepin-admin-web:80（管理端前端）
          └── /          → aideepin-user-web:80（用户端前端）
```

### 配置

编辑 `.env` 文件：

| 变量 | 说明 | 默认值 |
|------|------|--------|
| TZ | 时区 | Asia/Shanghai |
| IMAGE_VERSION | 后端镜像版本 | v1.1.0-dev |
| APP_VERSION | 应用版本 | 1.0.0-SNAPSHOT |
| APP_PROFILE | Spring 环境（dev/prod） | dev |
| JAVA_OPTS | JVM 参数 | -Xms1024m -Xmx2048m ... |
| ADI_DB_HOST | PostgreSQL 主机 | localhost |
| ADI_DB_PORT | PostgreSQL 端口 | 5432 |
| ADI_DB_NAME | PostgreSQL 数据库名 | aideepin |
| ADI_DB_USERNAME | PostgreSQL 用户名 | your-db-account |
| ADI_DB_PASSWORD | PostgreSQL 密码 | your-db-password |
| ADI_REDIS_HOST | Redis 主机 | redis |
| ADI_REDIS_PORT | Redis 端口 | 6379 |
| ADI_MAIL_HOST | SMTP 邮件主机 | your-email-host |
| ADI_MAIL_USERNAME | SMTP 邮件用户名 | your-email-username |
| ADI_MAIL_PASSWORD | SMTP 邮件密码 | your-email-password |
| ADI_ENCRYPT_AES_KEY | AES 加密密钥（16 位字符） | your-16-char-key |
| ADI_NEO4J_HOST | Neo4j 主机（可选） | localhost |
| ADI_NEO4J_PORT | Neo4j 端口（可选） | 7687 |
| ADI_NEO4J_USERNAME | Neo4j 用户名（可选） | neo4j |
| ADI_NEO4J_PASSWORD | Neo4j 密码（可选） | your-neo4j-password |
| ADI_NEO4J_DATABASE | Neo4j 数据库名（可选） | neo4j |

### 独立部署

如果需要将各服务部署到不同服务器，可以使用各子项目独立的 docker compose 配置：

- `server/docker/` — 后端服务 + Redis
- `user-web/docker/` — 用户端前端 + Nginx
- `admin-web/docker/` — 管理端前端 + Nginx

例如，将管理端部署在内网、用户端部署在公网时，分别在对应目录下执行：

```bash
cd <子项目>/docker
docker compose --env-file .env.prod up -d --build
```
