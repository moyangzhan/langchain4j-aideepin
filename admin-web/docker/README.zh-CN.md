> 中文文档 | **[🇬🇧 English](README.md)**

如需一键启动全部服务，请使用根目录的 [docker/](../../docker/README.zh-CN.md)。

## 管理端 - Docker 部署

### 使用

```bash
# 在 admin-web 目录下执行
cd docker

# 构建并启动
docker compose up -d --build

# 查看运行状态
docker ps

# 停止
docker compose down
```

### 说明

- 构建管理端前端并启动 Nginx 容器提供服务
- 修改 `nginx/nginx.conf` 中的 `proxy_pass` 为实际后端服务地址
- 访问地址：`http://your-ip:80/admin/`
- 本配置仅启动管理端前端
