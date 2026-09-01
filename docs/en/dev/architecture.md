# System Architecture

## 1. System Overview

AIDeepIn is an AI-powered productivity platform that helps enterprises and teams improve work efficiency through capabilities such as chat, knowledge bases, workflows, and image generation.

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                            User Layer                                       │
├──────────────────────────┬──────────────────────────────────────────────────┤
│   langchain4j-aideepin-web    │         langchain4j-aideepin-admin          │
│      (User Frontend Vue3)     │         (Admin Frontend Vue3)               │
│   • AI Chat / Drawing         │         • User Management                   │
│   • Knowledge Base Q&A        │         • Model Configuration               │
│   • Workflow Usage            │         • System Settings                   │
└──────────────────────────┴──────────────────────────────────────────────────┘
                                       │
                                       ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                      API Gateway Layer (Spring Boot)                        │
├──────────────────────────┬──────────────────────────────────────────────────┤
│        adi-chat               │              adi-admin                       │
│     (User API endpoints)      │         (Admin API endpoints)                │
└──────────────────────────┴──────────────────────────────────────────────────┘
                                       │
                                       ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                      Core Business Layer (adi-common)                       │
├──────────────┬──────────────┬──────────────┬──────────────┬─────────────────┤
│  Chat Service │  RAG Service │  Workflow    │ Image Gen    │  MCP Service    │
│               │              │  Engine      │ Service      │                 │
├──────────────┴──────────────┴──────────────┴──────────────┴─────────────────┤
│                    Model Service Layer (languagemodel)                      │
├─────────────┬─────────────┬─────────────┬─────────────┬────────────────────┤
│   OpenAI    │  DeepSeek   │  DashScope  │ SiliconFlow │     Ollama         │
└─────────────┴─────────────┴─────────────┴─────────────┴────────────────────┘
                                       │
                                       ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                         Data Storage Layer                                   │
├─────────────────────┬─────────────────────┬─────────────────────────────────┤
│   PostgreSQL        │       Redis         │       Neo4j (optional)          │
│   (pgvector+AGE)    │      (cache)         │       (graph database)         │
└─────────────────────┴─────────────────────┴─────────────────────────────────┘
```

## 2. Business Modules

### 2.1 Smart Chat

The core module of the system, supporting multi-turn conversations with AI.

**Chat Flow**:

```
User input (text/voice/image)
    ↓
Pre-processing (voice-to-text, quota validation)
    ↓
Context enhancement (short-term/long-term memory + linked knowledge base → RAG retrieval / linked MCP → tool calls)
    ↓
LLM generates answer (streaming output)
    ↓
Post-processing (TTS synthesis, token billing, save history)
```

**Core Capabilities**:
- Multi-model switching: Users can select different AI models in a conversation
- Multi-turn memory: Short-term memory (current session context) + Long-term memory (key information across sessions)
- Multimodal input: Text, images, voice
- TTS synthesis: Convert AI responses to speech output
- RAG-enhanced responses: When linked to a knowledge base, AI generates answers based on retrieved content
- MCP tool calling: External tools can be invoked during conversations (e.g. search engines, database queries)
- Multi-answer comparison: Multiple AI responses can be generated for the same question

### 2.2 Knowledge Base (RAG)

After importing documents into a knowledge base, AI chat can generate more accurate answers based on the knowledge base content.

**RAG Architecture**:

```
┌─────────────┐    ┌─────────────┐    ┌─────────────┐
│   Document  │ -> │   Document  │ -> │  Text       │
│   Upload    │    │   Parsing   │    │  Chunking   │
└─────────────┘    └─────────────┘    └─────────────┘
                                             │
                                             ▼
┌─────────────┐    ┌─────────────┐    ┌─────────────┐
│   Vector    │ <- │  Embedding  │ <- │  Text       │
│   Storage   │    │             │    │  Chunks     │
└─────────────┘    └─────────────┘    └─────────────┘
       │
       ▼
┌─────────────┐    ┌─────────────┐    ┌─────────────┐
│   User      │ -> │  Similarity │ -> │   Context   │
│   Question  │    │  Search     │    │  Building   │
└─────────────┘    └─────────────┘    └─────────────┘
                                             │
                                             ▼
                                      ┌─────────────┐
                                      │  LLM        │
                                      │  Generation │
                                      └─────────────┘
```

**Document Processing Flow**:

```
Upload documents (PDF/Word/TXT etc.)
    ↓
Parse document content, split into segments
    ↓
Index (optional: vector index, graph index, or both)
    ↓
User asks a question
    ↓
Retrieve relevant segments (vector similarity search / graph relationship query)
    ↓
Inject retrieved results into LLM prompt, generate answer
```

**Index Types**:
- **Vector Index**: Converts text to vectors and retrieves via semantic similarity. Stored in pgvector
- **Graph Index**: Extracts entities and relationships from documents to build a knowledge graph. Stored in Apache AGE
- **Hybrid Retrieval**: Uses both vector and graph retrieval with combined ranking

**Knowledge Base Configuration**:
- Public/Private: Public knowledge bases can be used by other users
- Retrieval parameters: Max recall count, minimum similarity threshold
- Chunking parameters: Document segment size, overlap length

### 2.3 Workflow

Build automated AI processing pipelines through visual orchestration of processing nodes.

**Node Types**:

| Node | Purpose |
|------|---------|
| Start Node | Workflow entry point, receives user input |
| Classification Node | Classifies input intent, determines the next branch |
| Knowledge Retrieval Node | Searches linked knowledge bases |
| LLM Answer Node | Calls an AI model to generate a response |
| HTTP Request Node | Calls an external API |
| Template Node | Generates structured output from a template |
| Switch/Conditional Node | Selects different branches based on conditions |
| Keyword Extraction Node | Extracts keywords from text |
| FAQ Extraction Node | Extracts Q&A pairs from text |
| Human Feedback Node | Pauses for human confirmation |
| Image Generation Node | Generates images |
| Email Node | Sends email |

**Workflow and Chat Relationship**: Users can invoke workflows during conversations. Workflows can internally search knowledge bases and call MCP tools.

### 2.4 MCP (Model Context Protocol)

MCP is a standardized protocol for integrating external tools, allowing AI to call external services during conversations.

**Supported Transport Methods**:
- SSE (Server-Sent Events)
- Stdio (standard I/O)
- Docker containers
- Remote services
- WASM (WebAssembly)

**Usage**:
- Administrators configure MCP services and their parameters
- Users enable MCP services in conversations
- AI automatically calls MCP tools as needed

### 2.5 Image Generation

Generate images from text descriptions:

```
User enters description → Select image model → Generate image → Save to gallery
```

- Supports multiple image generation platforms (OpenAI, DashScope, SiliconFlow, etc.)
- Generated images can be set as public (with watermark) or private
- Supports likes and comments

## 3. Module Relationships

```
                              ┌──────────┐
                              │ User Web │
                              └────┬─────┘
                                   │ HTTP / SSE
                              ┌────▼─────┐
                              │ Backend  │
                              └────┬─────┘
                                   │
     ┌─────────┬─────────┬─────────┼─────────┬─────────┐
     │         │         │         │         │         │
┌────▼───┐┌───▼────┐┌───▼────┐┌──▼────┐┌───▼─────┐
│  Chat  ││ Image  ││Workflow││Knowledge││   MCP   │
│        ││Generation││      ││  Base  ││         │
└────┬───┘└───┬────┘└──┬────┘└───┬────┘└───┬─────┘
     │        │         │         │         │
     └────────┴────┬────┴─────────┴────┬────┘
                   │                   │
              ┌────▼────┐         ┌────▼────┐
              │LLM      │         │RAG      │
              │Services │         │Retrieval│
              │(multi-  │         │(vector+ │
              │platform)│         │graph)   │
              └─────────┘         └─────────┘
```

**Interaction Relationships**:
- **Chat ↔ Knowledge Base**: Conversations can link to knowledge bases for RAG-enhanced responses
- **Chat ↔ MCP**: Conversations can invoke MCP external tools
- **Workflow ↔ Knowledge Base**: Workflow nodes can search knowledge bases
- **Workflow ↔ MCP**: Workflow nodes can call MCP services
- **Workflow ↔ Image Generation**: Workflows include image generation nodes
- **Workflow ↔ Chat**: Workflow nodes can call LLM to generate answers

## 4. Users & Permissions

**User Model**:
- Regular users: Register via email, use system features
- Administrators: Manage system configuration, users, models, etc. via the admin panel

**Quota Controls**:
- Daily token usage limits
- Request rate limits
- Image generation quantity limits

**Privacy Controls**:
- Conversations, knowledge bases, workflows, and images can all be set as public or private
- Public content can be viewed and used by other users

## 5. Admin Features

| Feature | Description |
|---------|-------------|
| Dashboard | System overview and monitoring |
| User Management | Create, edit users, set quotas |
| Conversation Management | View all user conversations, manage preset conversation templates |
| Knowledge Base Management | View and manage all knowledge bases |
| Model Management | Configure model platforms and AI models |
| MCP Management | Configure MCP services |
| Workflow Management | Manage workflow components |
| System Settings | Storage config, TTS/ASR config, quota config, rate limiting |
