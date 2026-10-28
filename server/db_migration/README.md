## Database Initialization

> **[🇨🇳 中文文档](README.zh-CN.md)** | English

### 1. Install PostgreSQL

For Ubuntu users, it's recommended to add the PGDG repository first (see [postgresql.org/download/linux/ubuntu](https://www.postgresql.org/download/linux/ubuntu/)), then install PostgreSQL along with the required extensions in one command:

```bash
sudo apt install -y \
  postgresql-16 \
  postgresql-16-pgvector \
  postgresql-16-age
```

> **Note**: Install extensions as needed. If you use neo4j to replace an extension, you don't need to install that extension.
>
> * pgvector: https://github.com/pgvector/pgvector (vector database, can be replaced by neo4j)
> * Apache AGE: https://github.com/apache/age (graph database, can be replaced by neo4j)

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

Upgrading this project is intended for deployers with development experience. Make sure you can:

* Work on the server via the command line and execute SQL (psql or a client tool)
* Treat the upgrade as an atomic operation: complete the "incremental migration SQL → backend → frontend" replacement within the same window

Starting from the number that corresponds to your current version, execute all incremental migration scripts (`NNN_*.sql`) in numeric order; do NOT run the `all_*` files.

For the complete upgrade procedure (backup, stopping services, migrations, and backend/frontend replacement), see [Deployment: Upgrade](../../docs/en/deployment/upgrade.md).

> **Note**: The default SCHEMA in DDL is `public`. Modify it if needed.
