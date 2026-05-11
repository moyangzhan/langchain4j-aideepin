## 数据库初始化

> 中文文档 | **[🇬🇧 English](README.md)**

### 1. 安装 PostgreSQL 扩展

> **注意**：扩展按需安装。如果使用 neo4j 替代某个扩展，则该扩展无需安装。

* pgvector：https://github.com/pgvector/pgvector（向量数据库，可用 neo4j 替代）
* Apache AGE：https://github.com/apache/age（图数据库，可用 neo4j 替代）

### 2. 启用扩展

> 仅安装了对应扩展的才需要执行。

```sql
CREATE EXTENSION IF NOT EXISTS vector;
CREATE EXTENSION IF NOT EXISTS age;
```

### 3. 创建数据库

```sql
CREATE DATABASE aideepin;
```

### 4. 执行 SQL 文件

文件命名规范：
* `all_*.sql` — 全量快照脚本，用于全新安装（始终为最新状态）
* `NNN_*.sql` — 增量迁移脚本，用于从旧版本升级

**全新安装：**

1. 执行 `all_ddl.sql` 创建所有表、触发器和注释
2. 执行 `all_dml.sql` 插入必需的基础数据（管理员账号、系统配置、模型平台、AI 模型）
3. 选择执行以下其中一个文件插入语言相关的展示数据（预设角色、工作流组件、MCP 服务）：
   * `all_dml_cn.sql` — 中文展示数据
   * `all_dml_en.sql` — 英文展示数据

**版本升级：**

按顺序执行增量迁移脚本（不需要执行 `all_*` 文件）：
* `001_3.21.0.sql`
* `002_update_deprecated_models.sql`
* `003_add_user_locale.sql`

> **注意**：DDL 中默认使用的 SCHEMA 为 `public`，请根据实际情况修改。
