## Getting Started

**LangChain4j-AIDeepin is an AI-based productivity enhancement tool.**

*It can be used to assist enterprises/teams in technical research and development, product design, HR/finance/IT information consulting, system/product consulting, customer service support, etc.*

> **🌟 If this project is helpful to you, please give it a star 🌟**

## System Composition and Documentation

[中文文档](README.md) | [English](README_en.md)

AIDEEPIN

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|__ Server (langchain4j-aideepin)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|__ User Web (langchain4j-aideepin-web)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|__ Admin Web (langchain4j-aideepin-admin)

👉[Detailed Documentation](https://github.com/moyangzhan/langchain4j-aideepin/wiki)

Backend source repository: [github](https://github.com/moyangzhan/langchain4j-aideepin) or [gitee](https://gitee.com/moyangzhan/langchain4j-aideepin)

Frontend projects:

* User Web: langchain4j-aideepin-web
    * [github](https://github.com/moyangzhan/langchain4j-aideepin-web)
    * [gitee](https://gitee.com/moyangzhan/langchain4j-aideepin-web)
* Admin Web: langchain4j-aideepin-admin
    * [github](https://github.com/moyangzhan/langchain4j-aideepin-admin)
    * [gitee](https://gitee.com/moyangzhan/langchain4j-aideepin-admin)

## Demo URL

[http://www.aideepin.com](http://www.aideepin.com/)

## Features

* Multi-conversation (multi-role)
* Image generation (text-to-image, image editing, image-to-image)
* Knowledge base based on large models (RAG)
  * Vector search
  * Graph search
* Network search based on large models (RAG)
* AI workflow
* MCP service marketplace
* ASR & TTS
  * Text question - Text response
  * Text question - Voice response
  * Voice question - Text response
  * Voice question - Voice response
* Long-term memory
* Storage
  * Local Storage
  * OSS (Alibaba Cloud)

## Integrated Platform Features

| Model Platform | Chat | Image Generation | Image Edit | Image variation | Background Generation | Image Recognition | Text-to-Speech | Speech Recognition |
|----------------|:-----|:-----------------| ----- |-----------------| ---------- | --------- |---------|-------------------------------|
| OpenAI         | ✓    | ✓                | ✓  | ✓               |          |          |         |                               |
| Dashscope      | ✓    | ✓                |     |                 | ✓       | ✓       | ✓       | ✓                             |
| SiliconFlow    | ✓    | ✓                |     |                 |          |      ✓   |     ✓   | ✓                             |
| Ollama         | ✓    |                  |    |                 |          |          |         |                               |
| DeepSeek       | ✓    |                  |     |                 |          |          |         |                               |
| Qianfan        | ✓    |                  |     |                 |          |          |         |                               |

## Tech Stack

This repository is for the backend service

Tech stack:

* JDK 17
* Spring Boot 3.0.5
* [langchain4j(Java version of LangChain)](https://github.com/langchain4j/langchain4j)
* [langgraph4j](https://github.com/bsorrentino/langgraph4j)
* Postgresql
    * pgvector extension: https://github.com/pgvector/pgvector
    * Apache AGE extension: https://github.com/apache/age
* [neo4j](https://neo4j.com/deployment-center/)

Frontend tech stack:

* vue3
* vite
* typescript
* pnpm
* pinia
* naiveui

## How to Deploy

### Initialization

**a. Initialize the database**

* Create the database `aideepin`
* Execute `docs/create.sql`
* Enable and configure the model platform (also referred to as model provider in some projects) or use the [admin web](https://github.com/moyangzhan/langchain4j-aideepin-admin) to configure on the interface

    * Configure model platforms
      ```plaintext

      -- DeepSeek
      update adi_model_platform set api_key = 'my_deepseek_secret_key' where name = 'deepseek';

      -- OpenAI
      update adi_model_platform set api_key = 'my_openai_secret_key' where name = 'openai';
  
      -- Dashscope
      update adi_model_platform set api_key = 'my_dashcope_api_key' where name = 'dashscope';

      -- Siliconflow
      update adi_model_platform set api_key = 'my_siliconflow_api_key' where name = 'siliconflow_setting';
  
      -- Qianfan API key and secret key
      update adi_model_platform set api_key = 'my_qianfan_api_key',secret_key='my_qianfan_secret_key' where name = 'qianfan';
  
      -- Ollama configuration
      update adi_model_platform set base_url = 'my_ollama_base_url' where name = 'ollama';
      ```
    * Enable model platform models or add new models
      ```
      -- Enable model
      update adi_ai_model set is_enable = true where name = 'deepseek-chat';
      update adi_ai_model set is_enable = true where name = 'gpt-3.5-turbo';
      update adi_ai_model set is_enable = true where name = 'dall-e-2';
      update adi_ai_model set is_enable = true where name = 'qwen-turbo';
      update adi_ai_model set is_enable = true where name = 'THUDM/GLM-Z1-9B-0414';
      update adi_ai_model set is_enable = true where name = 'ernie_speed';
      update adi_ai_model set is_enable = true where name = 'tinydolphin';
  
      -- Add new model
      INSERT INTO adi_ai_model (name, type, platform, is_enable) VALUES ('vicuna', 'text', 'ollama', true);
      ```
* Fill in the search engine configuration

    * Google configuration
      ```
      update adi_sys_config set value = '{"url":"https://www.googleapis.com/customsearch/v1","key":"my key from cloud.google.com","cx":"my cx from programmablesearchengine.google.com"}' where name = 'google_setting';
      ```

**b. Modify the configuration file**

* postgresql: `application-[dev|prod].xml` in `spring.datasource`
* redis: `application-[dev|prod].xml` in `spring.data.redis`
* mail: `application.xml` in `spring.mail`

### Build and Run

* Enter the project

  ```plaintext
  cd langchain4j-aideepin
  ```
* Package:

  ```
  mvn clean package -Dmaven.test.skip=true
  ```
* Run

    * Start with jar:

  ```plaintext
  cd adi-bootstrap/target
  nohup java -jar -Xms768m -Xmx1024m -XX:+HeapDumpOnOutOfMemoryError adi-bootstrap-0.0.1-SNAPSHOT.jar --spring.profiles.active=[dev|prod] dev/null 2>&1 &
  ```

    * Start with docker

  ```plaintext
  cd adi-bootstrap
  docker build . -t aideepin:0.0.1
  docker run -d \
    --name=aideepin \
    -p 8888:9999 \
    -e APP_PROFILE=[dev|prod] \
    -v="/data/aideepin/logs:/data/logs" \
    aideepin:0.0.1
  ```
## Contributing Guidelines
All forms of contributions are welcome, including but not limited to:
* Submitting Bug Reports
* Proposing New Features
* Improving Documentation
* Submitting Code (PR)

Code Submission Process:
1. Fork this repository
1. Create a feature branch (git checkout -b feature/xxx)
1. Commit changes (git commit -m 'feat: xxx')
1. Push the branch (git push origin feature/xxx)
1. Submit a Pull Request
   
## Screenshots

**AI Chat:**
![1691583184761](image/README/1691583184761.png)

**AI Drawing:**
![draw_002](image/README/draw_002.png "AI Drawing")

**Knowledge Base:**
![kb01](image/README/kb01.png)

**Vectorization:**
![kb03](image/README/kb03.png)

**Knowledge Graph:**
![kb_graph_01](image/README/kb_graph_01.png)

**Workflow:**
![workflow_01](image/README/workflow.png)

## Recommended Projects
[Mango Desk](https://github.com/moyangzhan/mango-desk) 

Mango Desk is a local-first desktop app for searching your local documents with natural language.

It helps you find information based on what you remember, not file names or folder structures.
