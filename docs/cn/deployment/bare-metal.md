# 裸机部署

> [← 部署](index.md) · [English](../../en/deployment/bare-metal.md)

无 Docker 环境的部署方式：后端以 JAR 直接运行，前端构建产物自行托管。

## 编译

```bash
cd server
mvn clean package -Dmaven.test.skip=true
```

## 配置

参照 `adi-bootstrap/src/main/resources/application-dev.yml.example`（配置项相同）创建 `application-prod.yml`，修改：

- PostgreSQL：`spring.datasource`
- Redis：`spring.data.redis`
- 邮箱：`application.yml` → `spring.mail`
- 向量数据库（默认 pgvector）：`adi.vector-database=[pgvector|neo4j]`
- 图数据库（默认 Apache AGE）：`adi.graph-database=[apache-age|neo4j]`

## 启动后端

```bash
cd adi-bootstrap/target
nohup java -jar -Xms768m -Xmx1024m -XX:+HeapDumpOnOutOfMemoryError \
  adi-bootstrap-0.0.1-SNAPSHOT.jar --spring.profiles.active=prod \
  > /dev/null 2>&1 &
```

## 部署前端

两个前端分别在 `user-web/`、`admin-web/` 目录内构建：

```bash
pnpm build
```

将 `dist/` 产物交给自建的 Nginx 等静态服务托管，并把 `/api/` 反向代理到后端 9999 端口（转发时去掉 `/api` 前缀）。Nginx 参考配置见 `docker/nginx/nginx.conf` 与 `user-web/README.zh-CN.md`。

---

上一篇：[Docker 独立部署](docker-split.md) ｜ 下一篇：[升级](upgrade.md)
