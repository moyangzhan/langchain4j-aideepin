## Getting Started

> **[🇨🇳 中文文档](README.zh-CN.md)** | English

**LangChain4j-AIDeepin — Enterprise-grade AI application platform.**

Integrates AI chat, knowledge base (RAG), workflow orchestration, long/short-term memory, MCP tools, and more — for rapidly building intelligent business assistants.

Demo: [http://www.aideepin.com](http://www.aideepin.com/)

## Repository Structure

| Directory | Description | Docs |
|:----------|:------------|:-----|
| server/ | Backend service ([Spring Boot](https://github.com/spring-projects/spring-boot) + [langchain4j](https://github.com/langchain4j/langchain4j) + [langgraph4j](https://github.com/bsorrentino/langgraph4j)) | [README](server/README.md) |
| admin-web/ | Admin dashboard ([Vue 3](https://github.com/vuejs/core) + [Naive UI](https://github.com/tusen-ai/naive-ui)) | [README](admin-web/README.md) |
| user-web/ | User-facing web app ([Vue 3](https://github.com/vuejs/core) + [Naive UI](https://github.com/tusen-ai/naive-ui)) | [README](user-web/README.md) |

## Features

| Feature | Description |
|:--------|:------------|
| AI Chat | Multi-role (multi-conversation) with configurable prompts, models, and parameters. Streaming output |
| Image Generation | Text-to-image and image editing. Supports GPT-Image-2, DashScope Wanx, and more |
| Knowledge Base (RAG) | Supports both vector search and knowledge graph retrieval |
| AI Workflow | Visual editor with conditional branching, parallel execution, and built-in nodes for LLM calls, knowledge base queries, human feedback, and more |
| MCP Service Marketplace | Integrate MCP services to extend AI with external tools and data sources |
| ASR & TTS | Flexible text/voice input and output combinations (text⇄text, text⇄voice, voice⇄text, voice⇄voice), with selectable AI voice tones |
| Short & Long-term Memory | Automatically extracts and stores key information from conversations for personalized responses based on historical context |
| Storage | Local file storage, Alibaba Cloud OSS integration |

## Integrated Platform Features

| Model Platform | Chat | Image Generation | Image Recognition | Text-to-Speech | Speech Recognition |
|:---------------|:----:|:----------------:|:-----------------:|:--------------:|:------------------:|
| OpenAI         |  ✓   |        ✓         |                   |                |                    |
| Qwen      |  ✓   |        ✓         |         ✓         |       ✓        |         ✓          |
| SiliconFlow    |  ✓   |        ✓         |         ✓         |       ✓        |         ✓          |
| Ollama         |  ✓   |                  |                   |                |                    |
| DeepSeek       |  ✓   |                  |                   |                |                    |

For detailed deployment instructions, see [docker/README.md](docker/README.md) or each sub-project's README.

## Contributing Guidelines

See [Contributing Guidelines](CONTRIBUTING.md) for details.

## ⭐ Support the Project

If you find LangChain4j-AIDeepin useful, please consider:

- Starring the repository on GitHub
- Recommending it to others
- Sharing your experience

## ❤️ Thanks to

**Contributors**

<a href="https://github.com/moyangzhan/langchain4j-aideepin/graphs/contributors">
  <img src="https://contrib.rocks/image?repo=moyangzhan/langchain4j-aideepin" width="50%" />
</a>

<br/>

**Infrastructure Sponsor:**

<a href="https://www.digitalocean.com/">
  <img src="https://opensource.nyc3.cdn.digitaloceanspaces.com/attribution/assets/SVG/DO_Logo_horizontal_blue.svg" width="201px">
</a>

## Recommended Projects

[Mango Finder](https://github.com/moyangzhan/mango-finder) — A local-first desktop app for semantic search across documents, images, and audio files using natural language, with cross-device search support. It helps you find information based on what you remember, not file names or folder structures.
