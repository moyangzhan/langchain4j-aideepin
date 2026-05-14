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

See [docker/README.md](docker/README.md) or each sub-project's README for detailed **deployment** instructions.

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

## Screenshots

<table>
  <tr>
    <td><img src="docs/image/README/1691583184761.png" alt="AI Chat" /></td>
    <td><img src="docs/image/README/adi-gallery.png" alt="Image Generation" /></td>
  </tr>
  <tr>
    <td align="center">AI Chat</td>
    <td align="center">Image Generation</td>
  </tr>
  <tr>
    <td><img src="docs/image/README/kb_graph_02.png" alt="Knowledge Base" /></td>
    <td><img src="docs/image/README/workflow.png" alt="Workflow" /></td>
  </tr>
  <tr>
    <td align="center">Knowledge Base</td>
    <td align="center">Workflow</td>
  </tr>
</table>

## ⭐ Support the Project

If you find LangChain4j-AIDeepin useful, please consider:

- Starring the repository on GitHub
- Recommending it to others
- Sharing your experience

## ❤️ Thanks to

**Contributors**

<a href="https://github.com/moyangzhan/langchain4j-aideepin/graphs/contributors">
  <img src="https://contrib.rocks/image?repo=moyangzhan/langchain4j-aideepin" width="25%" />
</a>

<br/>

**Infrastructure Sponsor:**

<a href="https://www.digitalocean.com/">
  <img src="https://opensource.nyc3.cdn.digitaloceanspaces.com/attribution/assets/SVG/DO_Logo_horizontal_blue.svg" width="150px">
</a>

## Recommended Projects

[Mango Finder](https://github.com/moyangzhan/mango-finder) — A local-first desktop app for semantic search across documents, images, and audio files using natural language, with cross-device search support. It helps you find information based on what you remember, not file names or folder structures.
