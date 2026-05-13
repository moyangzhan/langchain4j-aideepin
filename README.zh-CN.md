## Getting Started

> 中文文档 | **[🇬🇧 English](README.md)**

**LangChain4j-AIDeepin（得应AI） 是基于 AI 的工作效率提升工具。**

*可用于辅助企业/团队进行技术研发、产品设计、人事/财务/IT信息咨询、系统/商品咨询、客服话术支撑等工作*

## 仓库结构

```
langchain4j-aideepin/
  ├── server/         后端服务（Spring Boot + langchain4j）
  ├── admin-web/      管理端 WEB（Vue 3 + Naive UI）
  └── user-web/       用户端 WEB（Vue 3 + Naive UI）
```

- [后端服务 README](server/README.zh-CN.md) — 技术栈、部署、配置
- [管理端 README](admin-web/README.zh-CN.md) — 管理端功能与构建
- [用户端 README](user-web/README.md) — 用户端功能与构建

## 体验网址

[http://www.aideepin.com](http://www.aideepin.com/)

## 功能点

### AI 对话

多会话、多角色，可配置提示词、模型和参数，支持流式输出。

### 图片生成

文生图、图片编辑，支持 GPT-Image-2、灵积万相等模型。

### 知识库（RAG）

上传文档构建知识库，支持向量搜索和知识图谱两种检索方式。

### AI 工作流

可视化编辑器，支持条件分支、并行执行，内置 LLM 调用、知识库查询、人工反馈等多种节点。

### MCP 服务市场

集成 MCP 服务，扩展 AI 的工具和数据源能力。

### ASR & TTS

支持灵活的语音输入输出组合：

- 文字提问 → 文字回复
- 文字提问 → 语音回复
- 语音提问 → 文字回复
- 语音提问 → 语音回复

AI 音色可选。

### 长短期记忆

自动从对话中提取和存储关键信息，使 AI 能够基于历史上下文进行个性化回复。

### 存储

- 本地存储
- 阿里云 OSS

## 已集成的模型平台功能

| 模型平台   | 对话 | 文生图 | 背景生成 | 图像识别 | 语音合成 | 语音识别 |
|:---------|:----:|:-----:|:-------:|:-------:|:-------:|:-------:|
| OpenAI   |  ✓   |   ✓   |         |         |         |         |
| 千问      |  ✓   |   ✓   |    ✓    |    ✓    |    ✓    |    ✓    |
| 硅基流动   |  ✓   |   ✓   |         |    ✓    |    ✓    |    ✓    |
| Ollama   |  ✓   |       |         |         |         |         |
| DeepSeek |  ✓   |       |         |         |         |         |

## 技术栈

后端：

- JDK 17
- Spring Boot 3.5.14
- [langchain4j](https://github.com/langchain4j/langchain4j)（Java 版 LangChain）
- [langgraph4j](https://github.com/bsorrentino/langgraph4j)
- PostgreSQL
  - [pgvector](https://github.com/pgvector/pgvector) 扩展（向量数据库）
  - [Apache AGE](https://github.com/apache/age) 扩展（图数据库）
- [neo4j](https://neo4j.com/deployment-center/)（pgvector + Apache AGE 的替代方案）

前端：

- Vue 3
- Vite
- TypeScript
- pnpm
- Pinia
- Naive UI

详细部署说明请查看 [docker/README.zh-CN.md](docker/README.zh-CN.md) 或各子项目的 README。

## 贡献指南

欢迎任何形式的贡献，包括但不限于：

- 提交 Bug 报告
- 提出功能建议
- 改进文档
- 提交代码（PR）

代码提交流程：

1. Fork 本仓库
2. 创建特性分支（`git checkout -b feature/xxx`）
3. 提交更改（`git commit -m 'feat: xxx'`）
4. 推送分支（`git push origin feature/xxx`）
5. 提交 Pull Request

## ⭐ 支持项目

如果 LangChain4j-AIDeepin 对您有帮助，欢迎：

- 给仓库点个 Star
- 推荐给身边的人
- 分享您的使用体验

## ❤️ 感谢

**Contributors**

<a href="https://github.com/moyangzhan/langchain4j-aideepin/graphs/contributors">
  <img src="https://contrib.rocks/image?repo=moyangzhan/langchain4j-aideepin" />
</a>

<br/>

**基础设施赞助：**

<a href="https://www.digitalocean.com/">
  <img src="https://opensource.nyc3.cdn.digitaloceanspaces.com/attribution/assets/SVG/DO_Logo_horizontal_blue.svg" width="201px">
</a>

## 推荐项目

[Mango Finder](https://github.com/moyangzhan/mango-finder) — 一款用自然语言搜索本地文件的桌面应用，支持文档、图片、音频的语义搜索及跨设备搜索功能。帮助您根据记忆中的内容查找信息，而不需要记住文件名或文件夹结构。
