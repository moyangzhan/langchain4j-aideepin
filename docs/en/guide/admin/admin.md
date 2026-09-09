# Admin Console

> [← User Guide](../index.md) · [简体中文](../../../cn/guide/admin/admin.md)

The admin console (admin-web) is for system administrators, deployed separately from the user web; sign in with an admin account. Sixteen pages across nine top-level menus: dashboard, users, model platforms, models, characters, preset characters, knowledge bases, MCP, workflow orchestration (components + flows), token monitoring, and system settings (storage / ASR / TTS / quota / rate limits).

## Dashboard

Six stat cards: **new users, new knowledge bases, new documents, characters, token consumption, image consumption** — each showing the daily delta plus total / monthly consumption.

## User Management

Search filters: name, status (pending activation / normal / disabled), created/updated time, admin flag.

Table: id, uuid, name, email, avatar, status, activation time, admin flag, etc.

**New user** form fields:

| Field | Description |
|---|---|
| Name (required), password | Create the account |
| Daily / monthly token quota | Token consumption caps |
| Daily / monthly request quota | LLM request caps |
| Daily / monthly image quota | Image generation caps |

Row actions: **Edit**, **Disable / Enable** (disable freezes login). When activation emails go missing, set the user's status to normal to **activate on behalf**.

## Model Platforms & Models

The admin's most-used feature — "empty model list" or "model unavailable" for users is usually solved here.

### Platform Configuration

Table: name, title, base URL, API Key (configured / not), Secret Key, proxy flag, OpenAI-compatible flag.

New / edit fields:

| Field | In-app hint (translated) |
|---|---|
| Name / title (required), base URL, API key, secret key | Platform basics; built-in options: Openai, Lingji, Qianfan, Ollama, DeepSeek, SiliconFlow |
| Enable proxy | Form note: "Detailed proxy configuration lives in the project's configuration files" |
| OpenAI API compatible | Form note: "OpenAI-API-compatible platforms need no extra code adaptation" — chat-type models integrate with zero code |

> [!WARNING]
> Deleting a platform confirms "cannot be recovered and may affect model calls…"; built-in default platforms cannot be deleted.

### Model Management

Toolbar: new, **Refresh status** (full health probe, "Health probe triggered").

Table: name, title, type, platform, enabled, **health** (🟢 normal / 🔴 abnormal / disabled), free flag, context window, max input/output tokens, etc.

Key form fields:

| Field | Description |
|---|---|
| Type (required) | Text / image / embedding / rerank / TTS / ASR |
| Platform (required) | Dropdown from platform configuration |
| Input types (required, multi) | Text / image / audio / video |
| Output formats (required, multi) | Text / JSON |
| Is reasoner / thinking closable / supports web search | Drives the [user-side toggles](../model/model.md#capability-differences) |
| Properties | JSON editor, e.g. TTS voices `{"voices":[...]}`, image sizes |
| Context window, max input/output tokens | Numbers |

Row actions: **edit, enable/disable, set free / set paid, delete**.

Integrating a brand-new platform (database config + optional code): [Model Platform Integration Guide](../../dev/model-platform-integration.md).

## Characters & Presets

- **Characters**: page banner reads "In this system a character is an AI persona"; browse all users' characters (title, persona, total tokens, context flag); edit/delete;
- **Preset characters**: maintain the user-side preset library: title, **type** (11 categories), description, persona, **knowledge base name** (placeholder: "Leave empty to skip auto-creating a knowledge base" — when set, copying the preset auto-creates a same-named dedicated base).

## Knowledge Base Management

Search: title, owner name, public flag, time.

Table: name, owner, document count, vector count, like count, public flag, plus the full configuration folded in a column: split overlap, split strategy, max tokens per segment, ingest model, max retrieval count, min retrieval score, LLM temperature, system prompt.

The edit dialog mirrors the user-side create form (custom-separator presets `\n\n (paragraph)`, `\n (newline)`; free input); edit only, no create.

## MCP Management

Register / maintain MCP services. Edit form:

| Field | In-app hints (translated) |
|---|---|
| Transport | SSE (tagged "deprecated" + alert: "SSE is deprecated by the new MCP spec; use Streamable HTTP for new services") / streamable-http / stdio |
| Install type | docker / local / remote / WebAssembly |
| SSE URL, SSE timeout (s, default 30) / STDIO command & args | Shown per transport |
| Parameters | Columns: **parameter name** ("must match the parameter name in the MCP Server"), **parameter title** ("display only, to convey meaning"), value, **sensitive** ("sensitive information is stored encrypted"); "+add parameter" |
| Parameter definitions | "Definitions users fill with actual values later" — drives the user-side configure dialog |
| Website, description, enabled | — |

## Workflow Orchestration

- **Components**: maintain canvas node components. Name hints: "1. The name is the component's unique identifier and cannot change after saving" / "2. Letters only (a-z, A-Z), no spaces"; display order: "components' order in the designer, ascending"; Start/End cannot be disabled or deleted;
- **Flows**: all workflows' title, public flag, enabled flag, creator; edit / toggle / delete.

## Token Monitoring

Read-only. Filters: username, platform, model name, **source type**, request time range.

Table: username, source type, platform, model, **input tokens, output tokens, duration (ms)**, request time.

Source types: unknown, character chat, KB Q&A, KB indexing, workflow node, agent, long-term memory extraction, long-term memory analysis — useful to locate where quota goes (indexing and memory extraction consume tokens too).

## System Settings

| Page | Description |
|---|---|
| **Storage** | Shows the current location (local / Aliyun OSS). Local: "files stored on the server's disk"; OSS: fill in access key id / secret / bucket / endpoint, then save and enable |
| **ASR** | Pick an ASR model (platform follows), max recognition duration (s), max file size (KB) |
| **TTS** | Banner (translated): "Synthesizer side — 1. Client: use the client's (e.g. browser) TTS, ignoring model/platform parameters, free. 2. Server: server-side synthesis via LLMs such as cosyvoice-v2, usually paid."; pick the side + TTS model |
| **Quota configuration** | Note: "**changes take effect within 10 minutes**". Four dimensions: tokens (daily/monthly max), requests (daily/monthly count), images (daily/monthly count), chat (daily/monthly questions) |
| **Rate limits** | Note: "changes take effect within 10 minutes". Text requests and image generation each set "requests + time window (minutes)" |

---

Previous: [Prompt Store](../prompt-store.md) · Next: [API Reference](../../api/index.md)
