## Getting Started

> **[🇨🇳 中文文档](README.zh-CN.md)** | English

**LangChain4j-AIDeepin is an AI-based productivity enhancement tool.**

*It can be used to assist enterprises/teams in technical research and development, product design, HR/finance/IT information consulting, system/product consulting, customer service support, etc.*

## Repository Structure

```
langchain4j-aideepin/
  ├── server/         Backend service (Spring Boot + langchain4j)
  ├── admin-web/      Admin dashboard (Vue 3 + Naive UI)
  └── user-web/       User-facing web app (Vue 3 + Naive UI)
```

- [Server README](server/README.md) — Tech stack, deployment, configuration
- [Admin Web README](admin-web/README.md) — Admin dashboard features and setup
- [User Web README](user-web/README.md) — User-facing web app features and setup

## Demo URL

[http://www.aideepin.com](http://www.aideepin.com/)

## Features

### AI Chat

Multi-conversation, multi-role support with configurable prompts, models, and parameters. Streaming output.

### Image Generation

Text-to-image and image editing. Supports GPT-Image-2, DashScope Wanx, and more.

### Knowledge Base (RAG)

Upload documents to build knowledge bases. Supports both vector search and knowledge graph retrieval.

### AI Workflow

Visual editor with conditional branching, parallel execution, and built-in nodes for LLM calls, knowledge base queries, human feedback, and more.

### MCP Service Marketplace

Integrate MCP services to extend AI with external tools and data sources.

### ASR & TTS

Full voice interaction support with flexible input/output combinations:

- Text question → Text response
- Text question → Voice response
- Voice question → Text response
- Voice question → Voice response

### Short & Long-term Memory

Automatically extracts and stores key information from conversations, allowing the AI to personalize responses based on historical context.

### Storage

- Local file storage
- Alibaba Cloud OSS integration

## Integrated Platform Features

| Model Platform | Chat | Image Generation | Background Generation | Image Recognition | Text-to-Speech | Speech Recognition |
|:---------------|:----:|:----------------:|:---------------------:|:-----------------:|:--------------:|:------------------:|
| OpenAI         |  ✓   |        ✓         |                       |                   |                |                    |
| Qwen      |  ✓   |        ✓         |           ✓           |         ✓         |       ✓        |         ✓          |
| SiliconFlow    |  ✓   |        ✓         |                       |         ✓         |       ✓        |         ✓          |
| Ollama         |  ✓   |                  |                       |                   |                |                    |
| DeepSeek       |  ✓   |                  |                       |                   |                |                    |

## Tech Stack

Backend:

- JDK 17
- Spring Boot 3.5.14
- [langchain4j](https://github.com/langchain4j/langchain4j) (Java version of LangChain)
- [langgraph4j](https://github.com/bsorrentino/langgraph4j)
- PostgreSQL
  - [pgvector](https://github.com/pgvector/pgvector) extension (vector database)
  - [Apache AGE](https://github.com/apache/age) extension (graph database)
- [neo4j](https://neo4j.com/deployment-center/) (alternative to pgvector + Apache AGE)

Frontend:

- Vue 3
- Vite
- TypeScript
- pnpm
- Pinia
- Naive UI

For detailed deployment instructions, see [docker/README.md](docker/README.md) or each sub-project's README.

## Contributing Guidelines

All forms of contributions are welcome, including but not limited to:

- Submitting Bug Reports
- Proposing New Features
- Improving Documentation
- Submitting Code (PR)

Code Submission Process:

1. Fork this repository
2. Create a feature branch (`git checkout -b feature/xxx`)
3. Commit changes (`git commit -m 'feat: xxx'`)
4. Push the branch (`git push origin feature/xxx`)
5. Submit a Pull Request

## ⭐ Support the Project

If you find LangChain4j-AIDeepin useful, please consider:

- Starring the repository on GitHub
- Recommending it to others
- Sharing your experience

## ❤️ Thanks to

**Contributors**

<a href="https://github.com/moyangzhan/langchain4j-aideepin/graphs/contributors">
  <img src="https://contrib.rocks/image?repo=moyangzhan/langchain4j-aideepin" />
</a>

<br/>

**Infrastructure Sponsor:**

<a href="https://www.digitalocean.com/">
  <img src="https://opensource.nyc3.cdn.digitaloceanspaces.com/attribution/assets/SVG/DO_Logo_horizontal_blue.svg" width="201px">
</a>

## Recommended Projects

[Mango Finder](https://github.com/moyangzhan/mango-finder) — A local-first desktop app for semantic search across documents, images, and audio files using natural language, with cross-device search support. It helps you find information based on what you remember, not file names or folder structures.
