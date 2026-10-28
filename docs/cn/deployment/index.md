# 部署

面向负责部署与升级 AIDeepIn 的开发人员：从环境准备、全新部署到版本升级的完整流程。

## 全新部署

1. [Docker Compose 一键部署](quick-start.md)：单机快速部署，后端 + Redis + 双前端 + Nginx 一条命令拉起
2. [Docker 独立部署](docker-split.md)：各服务拆开、各自 compose 独立部署（如管理端放内网、用户端放公网）
3. [裸机部署](bare-metal.md)：无 Docker 环境：后端以 JAR 直跑，前端构建产物自行托管

## 升级

4. [升级](upgrade.md)：备份、停服、数据库迁移、服务端与前端替换的原子流程；数据库不支持回退，请先在测试环境演练

---

> [← 返回总目录](../index.md) · [English](../../en/deployment/index.md)

