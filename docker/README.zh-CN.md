## Docker Compose 快速启动

> 中文文档 | **[🇬🇧 English](README.md)**

一条命令启动所有服务（后端、Redis、用户端、管理端、Nginx）。

### 前置条件

- 已安装 Docker 和 Docker Compose
- 已初始化 PostgreSQL 数据库（见 [server/README](../server/README.zh-CN.md)）

### 使用

```bash
cd docker

# 构建并启动所有服务
docker-compose up -d --build

# 查看日志
docker-compose logs -f

# 停止所有服务
docker-compose down
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

### 独立部署

如果需要将各服务部署到不同服务器，可以使用各子项目独立的 docker-compose 配置：

- `server/docker/` — 后端服务 + Redis
- `user-web/docker/` — 用户端前端 + Nginx
- `admin-web/docker/` — 管理端前端 + Nginx

例如，将管理端部署在内网、用户端部署在公网时，分别在对应目录下执行：

```bash
cd <子项目>/docker-compose
docker-compose up -d --build
```
