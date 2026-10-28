# Docker Compose 一键部署

> [← 部署](index.md) · [English](../../en/deployment/quick-start.md)

使用项目根目录的 `docker/` 一条命令拉起全部服务：后端（aideepin-api）、Redis、用户端、管理端、Nginx。PostgreSQL 不在 compose 内，需先自行安装并初始化。

## 1. 初始化数据库

### 安装 PostgreSQL 与扩展

Ubuntu 建议先添加 [PGDG 源](https://www.postgresql.org/download/linux/ubuntu/)，然后一键安装 PostgreSQL 及所需扩展：

```bash
sudo apt install -y postgresql-16 postgresql-16-pgvector postgresql-16-age
```

> [!NOTE]
> 扩展按需安装：pgvector（向量检索）与 Apache AGE（知识图谱）都可以用 neo4j 替代，改用 neo4j 的部分无需安装对应扩展。

启用扩展并创建数据库：

```sql
CREATE EXTENSION IF NOT EXISTS vector;
CREATE EXTENSION IF NOT EXISTS age;
CREATE DATABASE aideepin;
```

### 执行初始化脚本

脚本位于 `server/db_migration/`，按顺序执行：

1. `all_ddl.sql` —— 创建所有表、触发器和注释
2. `all_dml.sql` —— 插入基础数据（管理员账号、系统配置、模型平台、AI 模型）
3. `all_dml_cn.sql` 或 `all_dml_en.sql` —— 二选一，按语言插入展示数据（预设角色、工作流组件、MCP 服务）

> [!NOTE]
> DDL 默认 SCHEMA 为 `public`，如有不同请先调整。脚本命名规范与说明见 [db_migration/README](https://github.com/moyangzhan/langchain4j-aideepin/blob/main/server/db_migration/README.zh-CN.md)。

## 2. 配置并启动

```bash
cd docker

# 修改 .env（compose 默认读取 .env；生产配置可指定 .env.prod）
# ⚠️ 必改项：ADI_DB_HOST、ADI_DB_USERNAME、ADI_DB_PASSWORD、
#    ADI_MAIL_HOST、ADI_MAIL_USERNAME、ADI_MAIL_PASSWORD、ADI_ENCRYPT_AES_KEY

# 内存 ≥ 8G 的服务器：直接并行构建
docker compose up -d --build

# 内存 < 8G 的服务器：顺序构建以避免 OOM
docker compose build aideepin-admin-web aideepin-user-web
docker compose build aideepin-api
docker compose up -d
```

常用命令：`docker compose logs -f` 查看日志；`docker compose down` 停止全部服务。

## 3. 服务架构

```
浏览器 → nginx:80
          ├── /api/      → aideepin-api:9999（后端服务）
          ├── /admin/    → aideepin-admin-web:80（管理端前端）
          └── /          → aideepin-user-web:80（用户端前端）
```

## 4. 首次初始化

启动后至少启用一个模型平台，普通用户才能正常使用。管理员账号由 `all_dml.sql` 初始化，登录管理端后：

- **推荐**：在管理端「平台配置」中填入模型平台的 API Key 并启用（见[管理端使用](../guide/admin/admin.md)）；
- 或直接 SQL 配置（示例）：

```sql
-- DeepSeek
UPDATE adi_model_platform SET api_key = 'my_deepseek_secret_key' WHERE name = 'deepseek';

-- OpenAI
UPDATE adi_model_platform SET api_key = 'my_openai_secret_key' WHERE name = 'openai';

-- 灵积
UPDATE adi_model_platform SET api_key = 'my_dashscope_api_key' WHERE name = 'dashscope';

-- 硅基流动
UPDATE adi_model_platform SET api_key = 'my_siliconflow_api_key' WHERE name = 'siliconflow_setting';

-- Ollama
UPDATE adi_model_platform SET base_url = 'my_ollama_base_url' WHERE name = 'ollama';
```

再启用需要的模型：

```sql
UPDATE adi_ai_model SET is_enable = true WHERE name = 'deepseek-v4-flash';
```

## 配置项参考（.env）

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

> [!TIP]
> 生产环境建议将 `APP_PROFILE` 设为 `prod`，并使用 `.env.prod` 管理配置。

---

上一篇：[部署](index.md) ｜ 下一篇：[Docker 独立部署](docker-split.md)
