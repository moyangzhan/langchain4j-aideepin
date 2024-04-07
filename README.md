## Getting Started

**LangChain4j-AIDeepin**
基于 ChatGPT 等大语言模型与 Langchain4j 等应用框架实现，开源、可离线部署的检索增强生成(RAG)大模型知识库项目。

> **该项目如对您有帮助，欢迎点赞**

## 体验网址

[http://www.aideepin.com](http://www.aideepin.com/)

## 功能点

* 注册&登录
* 多会话（多角色）
* 图片生成（文生图、修图、图生图）
* 提示词
* 额度控制
* 基于大模型的知识库（RAG）
* 基于大模型的搜索（RAG）
* 多模型随意切换
* 多搜索引擎随意切换

## 接入的模型：

* ChatGPT 3.5
* 通义千问
* 文心一言
* ollama
* DALL-E 2

## 接入的搜索引擎

Google

Bing (TODO)

百度 (TODO)

## 技术栈

该仓库为后端服务，前端项目见[langchain4j-aideepin-web](https://github.com/moyangzhan/langchain4j-aideepin-web)

后端：

jdk17

springboot3.0.5

[langchain4j(Java version of LangChain)](https://github.com/langchain4j/langchain4j)

**Postgresql(需要安装[pgvector](https://github.com/pgvector/pgvector)扩展)**

前端：

vue3+typescript+pnpm

## 如何部署

### 初始化

**a. 初始化数据库**

* 创建数据库aideepin
* 执行docs/create.sql
* 填充各模型的配置(至少设置一个)

openai的secretKey

```plaintext
update adi_sys_config set value = '{"secret_key":"my_openai_secret_key","models":["gpt-3.5-turbo"]}' where name = 'openai_setting';
```

灵积大模型平台的apiKey

```plaintext
update adi_sys_config set value = '{"api_key":"my_dashcope_api_key","models":["my model name,eg:qwen-max"]}' where name = 'dashscope_setting';
```

千帆大模型平台的配置

```plaintext
update adi_sys_config set value = '{"api_key":"my_qianfan_api_key","secret_key":"my_qianfan_secret_key","models":["my model name,eg:ERNIE-Bot"]}' where name = 'qianfan_setting';
```

ollama的配置

```
update adi_sys_config set value = '{"base_url":"my_ollama_base_url","models":["my model name,eg:tinydolphin"]}' where name = 'ollama_setting';
```

* 填充搜索引擎的配置

Google的配置

```
update adi_sys_config set value = '{"url":"https://www.googleapis.com/customsearch/v1","key":"my key from cloud.google.com","cx":"my cx from programmablesearchengine.google.com"}' where name = 'google_setting';
```


**b. 修改配置文件**

* postgresql: application-[dev|prod].xml中的spring.datasource
* redis: application-[dev|prod].xml中的spring.data.redis
* mail: application.xml中的spring.mail

### 编译及运行

* 进入项目

```plaintext
cd langchain4j-aideepin
```

* 打包：

```
mvn clean package -Dmaven.test.skip=true
```

* 运行

a. jar包启动：

```plaintext
cd adi-bootstrap/target
nohup java -jar -Xms768m -Xmx1024m -XX:+HeapDumpOnOutOfMemoryError adi-chat-0.0.1-SNAPSHOT.jar --spring.profiles.active=[dev|prod] dev/null 2>&1 &
```

b. docker启动

```plaintext
cd adi-bootstrap
docker build . -t aideepin:0.0.1
docker run -d \
  --name=aideepin \
  -e APP_PROFILE=[dev|prod] \
  -v="/data/aideepin/logs:/data/logs" \
  aideepin:0.0.1
```

## 待办：

增强RAG

增加搜索引擎（BING、百度）

## 截图

**AI聊天：**
![1691583184761](image/README/1691583184761.png)

**AI画图：**

![1691583124744](image/README/1691583124744.png "AI绘图")

**知识库：**
![kbindex](image/README/kbidx.png)

![kb01](image/README/kb01.png)

**向量化：**

![kb02](image/README/kb02.png)

![kb03](image/README/kb03.png)

**额度统计：**

![1691583329105](https://file+.vscode-resource.vscode-cdn.net/e%3A/WORKSPACE/aideepin/image/README/1691583329105.png "token统计")
