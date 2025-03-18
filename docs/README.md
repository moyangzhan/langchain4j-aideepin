## 数据库初始化

一、安装 PostgreSQL 扩展
pgvector：https://github.com/pgvector/pgvector
Apache AGE：https://github.com/apache/age

二、执行以下命令

```sql
CREATE EXTENSION IF NOT EXISTS vector;
CREATE EXTENSION IF NOT EXISTS age;
```

三、确定SCHEMA

create.sql 中的选择的 SCHEMA 为 `public`，请根据实际情况修改

四、执行 create.sql 以创建数据表
