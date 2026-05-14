## Getting Started

> 中文文档 | **[🇬🇧 English](README.md)**

本目录为后端服务。项目总览请查看[根目录 README](../README.zh-CN.md)。

## 技术栈

- JDK 17
- Spring Boot 3.5.14
- [langchain4j](https://github.com/langchain4j/langchain4j)（Java 版 LangChain）
- [langgraph4j](https://github.com/bsorrentino/langgraph4j)
- PostgreSQL
  - [pgvector](https://github.com/pgvector/pgvector) 扩展（向量数据库）
  - [Apache AGE](https://github.com/apache/age) 扩展（图数据库）
- [neo4j](https://neo4j.com/deployment-center/)（pgvector + Apache AGE 的替代方案）

## 开发及本地运行

### 初始化数据库

1. 创建数据库 `aideepin`
2. 执行 `db_migration/all_ddl.sql` 创建表结构
3. 执行 `db_migration/all_dml.sql` 插入基础数据
4. 执行 `db_migration/all_dml_cn.sql`（中文）或 `db_migration/all_dml_en.sql`（英文）插入展示数据
5. 配置并启用模型平台（至少启用一个，可参考[根目录 README](../README.zh-CN.md)中的表格选择），或使用[管理端](../admin-web/README.zh-CN.md)在界面上配置

配置模型平台：

```sql
-- DeepSeek
UPDATE adi_model_platform SET api_key = 'my_deepseek_secret_key' WHERE name = 'deepseek';

-- OpenAI
UPDATE adi_model_platform SET api_key = 'my_openai_secret_key' WHERE name = 'openai';

-- 灵积
UPDATE adi_model_platform SET api_key = 'my_dashcope_api_key' WHERE name = 'dashscope';

-- 硅基流动
UPDATE adi_model_platform SET api_key = 'my_siliconflow_api_key' WHERE name = 'siliconflow_setting';

-- Ollama
UPDATE adi_model_platform SET base_url = 'my_ollama_base_url' WHERE name = 'ollama';
```

启用模型或新增模型：

```sql
-- 启用模型
UPDATE adi_ai_model SET is_enable = true WHERE name = 'deepseek-v4-flash';
UPDATE adi_ai_model SET is_enable = true WHERE name = 'gpt-5-mini';
UPDATE adi_ai_model SET is_enable = true WHERE name = 'gpt-image-2';
UPDATE adi_ai_model SET is_enable = true WHERE name = 'qwen-turbo';
UPDATE adi_ai_model SET is_enable = true WHERE name = 'THUDM/GLM-Z1-9B-0414';
UPDATE adi_ai_model SET is_enable = true WHERE name = 'tinydolphin';

-- 新增模型
INSERT INTO adi_ai_model (name, type, platform, is_enable) VALUES ('vicuna', 'text', 'ollama', true);
```

配置搜索引擎（可选，工作流 Google 搜索节点需要）：

```sql
UPDATE adi_sys_config SET value = '{"url":"https://www.googleapis.com/customsearch/v1","key":"my_google_api_key","cx":"my_cx"}' WHERE name = 'google_setting';
```

### 修改配置文件

复制示例配置并重命名：

```bash
cp adi-bootstrap/src/main/resources/application-dev.yml.example adi-bootstrap/src/main/resources/application-dev.yml
```

然后修改以下配置项：

- PostgreSQL：`application-dev.yml` → `spring.datasource`
- Redis：`application-dev.yml` → `spring.data.redis`
- 邮箱：`application.yml` → `spring.mail`
- 向量数据库（默认 pgvector）：`application-dev.yml` → `adi.vector-database=[pgvector|neo4j]`
- 图数据库（默认 Apache AGE）：`application-dev.yml` → `adi.graph-database=[apache-age|neo4j]`

### 编译及运行

```bash
cd server
mvn clean package -Dmaven.test.skip=true
```

本地启动（开发模式）：

```bash
cd adi-bootstrap
mvn spring-boot:run -Dspring-boot.run.profiles=dev
```

或使用 JAR 包启动：

```bash
cd adi-bootstrap/target
java -jar adi-bootstrap-0.0.1-SNAPSHOT.jar --spring.profiles.active=dev
```

## 生产环境部署

### 初始化数据库

与[开发环境](#初始化数据库)相同。

### 修改配置文件

配置项与[开发环境](#修改配置文件)相同，配置文件为 `application-prod.yml`。

### 部署

#### 方式一：JAR 包

编译：

```bash
cd server
mvn clean package -Dmaven.test.skip=true
```

启动：

```bash
cd adi-bootstrap/target
nohup java -jar -Xms768m -Xmx1024m -XX:+HeapDumpOnOutOfMemoryError \
  adi-bootstrap-0.0.1-SNAPSHOT.jar --spring.profiles.active=prod \
  /dev/null 2>&1 &
```

#### 方式二：Docker

只拉起后端服务 + Redis，详见 [server/docker/README.zh-CN.md](docker/README.zh-CN.md)。

一键拉起全部服务（含前端），详见 [docker/README.zh-CN.md](../docker/README.zh-CN.md)。
