## Database Initialization

> **[🇨🇳 中文文档](README.zh-CN.md)** | English

### 1. Install PostgreSQL Extensions

> **Note**: Install extensions as needed. If you use neo4j to replace an extension, you don't need to install that extension.

* pgvector: https://github.com/pgvector/pgvector (vector database, can be replaced by neo4j)
* Apache AGE: https://github.com/apache/age (graph database, can be replaced by neo4j)

### 2. Enable Extensions

> Only execute for extensions you have installed.

```sql
CREATE EXTENSION IF NOT EXISTS vector;
CREATE EXTENSION IF NOT EXISTS age;
```

### 3. Create Database

```sql
CREATE DATABASE aideepin;
```

### 4. Execute SQL Files

File naming convention:
* `all_*.sql` — Full snapshot scripts for fresh install (always up-to-date)
* `NNN_*.sql` — Incremental migration scripts for upgrading from earlier versions

**Fresh Install:**

1. Execute `all_ddl.sql` to create all tables, triggers, and comments
2. Execute `all_dml.sql` to insert required base data (admin account, system config, model platforms, AI models)
3. Execute one of the following to insert language-specific display data (conversation presets, workflow components, MCP servers):
   * `all_dml_en.sql` — English display data
   * `all_dml_cn.sql` — Chinese display data

**Upgrade (from an earlier version):**

Execute the migration scripts in sequence (do NOT run the `all_*` files):
* `001_3.21.0.sql`
* `002_update_deprecated_models.sql`
* `003_add_user_locale.sql`

> **Note**: The default SCHEMA in DDL is `public`. Modify it if needed.
