## Getting Started

**LangChain4j-AIDeepin 是基于AI的企业内部提效工具。**

 *可用于辅助技术研发、产品设计、规章制度咨询、系统或商品咨询、客服话术支撑等工作*

> **🌟该项目如对您有帮助，欢迎点赞🌟**

## 系统组成及文档

AIDEEPIN

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|__ 服务端(langchain4j-aideepin)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|__ 用户端WEB(langchain4j-aideepin-web)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|__ 管理端WEB(langchain4j-aideepin-admin)

👉[详细文档](https://github.com/moyangzhan/langchain4j-aideepin/wiki)

服务端代码地址：[github](https://github.com/moyangzhan/langchain4j-aideepin)  [gitee](https://gitee.com/moyangzhan/langchain4j-aideepin)

前端项目：

* 用户端WEB：langchain4j-aideepin-web
  * [github](https://github.com/moyangzhan/langchain4j-aideepin-web)
  * [gitee](https://gitee.com/moyangzhan/langchain4j-aideepin-web)
* 管理端WEB：langchain4j-aideepin-admin
  * [github](https://github.com/moyangzhan/langchain4j-aideepin-admin)
  * [gitee](https://gitee.com/moyangzhan/langchain4j-aideepin-admin)

## 体验网址

[http://www.aideepin.com](http://www.aideepin.com/)

## 功能点

* 注册&登录
* 多会话（多角色）
* 图片生成（文生图、修图、图生图）
* 提示词
* 额度控制
* 基于大模型的知识库（RAG）
  * 向量搜索
  * 图搜索
* 基于大模型的搜索（RAG）
* 多模型随意切换
* 多搜索引擎随意切换

## 接入的模型：

* ChatGPT 3.5
* 通义千问
* 文心一言
* ollama
* DALL-E 2
* DALL-E 3

## 接入的搜索引擎

Google

Bing (TODO)

百度 (TODO)

## 技术栈

该仓库为后端服务

技术栈：

* jdk17
* springboot3.0.5
* [langchain4j(Java version of LangChain)](https://github.com/langchain4j/langchain4j)
* Postgresql
  * pgvector扩展：https://github.com/pgvector/pgvector
  * Apage AGE扩展：https://github.com/apache/age

前端技术栈：

* vue3
* vite
* typescript
* pnpm
* pinia
* naiveui

## 如何部署

### 初始化

**a. 初始化数据库**

* 创建数据库aideepin
* 执行docs/create.sql
* 配置模型(至少设置一个) 或者 使用[管理端](https://github.com/moyangzhan/langchain4j-aideepin-admin)在界面上配置

  * 配置AI平台
    ```plaintext
    -- openai的secretKey
    update adi_sys_config set value = '{"secret_key":"my_openai_secret_key"}' where name = 'openai_setting';

    -- 灵积大模型平台的apiKey
    update adi_sys_config set value = '{"api_key":"my_dashcope_api_key"}' where name = 'dashscope_setting';

    -- 千帆大模型平台的配置
    update adi_sys_config set value = '{"api_key":"my_qianfan_api_key","secret_key":"my_qianfan_secret_key"}' where name = 'qianfan_setting';

    -- ollama的配置
    update adi_sys_config set value = '{"base_url":"my_ollama_base_url"}' where name = 'ollama_setting';
    ```
  * 启用AI平台下的模型或新增模型
    ```
    -- Enable model
    update adi_ai_model set is_enable = true where name = 'gpt-3.5-turbo';
    update adi_ai_model set is_enable = true where name = 'dall-e-2';
    update adi_ai_model set is_enable = true where name = 'qwen-turbo';
    update adi_ai_model set is_enable = true where name = 'ernie_speed';
    update adi_ai_model set is_enable = true where name = 'tinydolphin';

    -- Add new model
    INSERT INTO adi_ai_model (name, type, platform, is_enable) VALUES ('vicuna', 'text', 'ollama', true);
    ```
* 填充搜索引擎的配置

  * Google的配置
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

  * jar包启动：

  ```plaintext
  cd adi-bootstrap/target
  nohup java -jar -Xms768m -Xmx1024m -XX:+HeapDumpOnOutOfMemoryError adi-bootstrap-0.0.1-SNAPSHOT.jar --spring.profiles.active=[dev|prod] dev/null 2>&1 &
  ```

  * docker启动

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

* 高级RAG
  * 查询压缩  √
  * 查询路由
  * Re-rank：支持本地rerank模型
* AI聊天
  * 多角色  √
  * 预设通用角色（管理后台创建）
* 图片模型：
  * DALL-E 2 & DALL-E 3 √
  * 聊天视图  √
  * 画廊视图  √
* 知识库：
  * 向量  √
  * 知识图谱  √
  * 文档召回数量可设置
    * 自动调整（根据LLM的上下文窗口大小）  √
    * 手动调整  √
  * 文档召回最低分数可设置  √
  * 切块时内容重叠数量可设置  √
  * 请求模型时temperature可设置  √
  * 严格模式与非严格模式  √
  * 答案来源  √
  * 支持拉取在线文档
  * FAQ
  * 评论
* 多模态支持
  * 图片
  * 音频
  * 视频
* 工具
  * FAQ提取
  * 文档对话
  * 翻译
* 搜索引擎
  * Google  √
  * Bing
  * 百度
* 额度统计及控制
  * 免费额度统计及限制
  * 计费额度统计及限制
  * 总额度统计
* 开放接口

## 截图

**AI聊天：**
![1691583184761](image/README/1691583184761.png)

**AI画图：**

![draw_001](image/README/draw_001.png "AI绘图")

![draw_002](image/README/draw_002.png "AI绘图")

**知识库：**
![kbindex](image/README/kbidx.png)

![kb01](image/README/kb01.png)

**向量化：**

![kb02](image/README/kb02.png)

![kb03](image/README/kb03.png)

**知识图谱：**

![kb_graph_01](image/README/kb_graph_01.png)

![kb_graph_02](image/README/kb_graph_02.png)

**额度统计：**

![1691583329105.png](image%2FREADME%2F1691583329105.png)
