# API Reference

> [← Back to contents](../index.md) · [简体中文](../../cn/api/index.md)

The AIDeepIn open API (`/ext/v1`) brings the platform's AI capabilities into your own programs: **character chat, knowledge base Q&A, draw tasks, MCP service lists and workflow runs**. Everything is self-service — generate an API key in the UI and start calling, no approval needed.

## Endpoint Overview

| Capability | Endpoint | Method | Key type |
|---|---|---|---|
| Character chat | `/ext/v1/character` | POST | Resource-level (bound to a character) |
| Knowledge base Q&A | `/ext/v1/knowledge` | POST | Resource-level (bound to a base) |
| Create draw task | `/ext/v1/draw/generation` | POST | User-level |
| Get draw task | `/ext/v1/draw/{uuid}` | GET | User-level |
| Supported MCP services | `/ext/v1/mcp/supported` | GET | User-level |
| Active MCP services | `/ext/v1/mcp/active` | GET | User-level |
| Run workflow | `/ext/v1/workflow/run` | POST | Resource-level (bound to a workflow) |

## Quick Example

```bash
curl -N -X POST "http://<host>:9999/ext/v1/character" \
  -H "Authorization: <your API key>" \
  -H "Content-Type: application/json" \
  -d '{"query": "Hi, introduce yourself", "response_mode": "streaming"}'
```

Start with [API Keys & Authentication](authentication.md).

## Endpoints

1. [API Keys & Authentication](authentication.md): the two key types, how to get them, auth header and errors
2. [Character Chat](character.md): chat as a character (streaming / blocking)
3. [Knowledge Base Q&A](knowledge-base.md): ask a knowledge base
4. [Draw Tasks](draw.md): create and query text-to-image tasks
5. [MCP Services](mcp.md): fetch the service list
6. [Workflow](workflow.md): trigger a workflow run

> The full machine-readable server API is available at Swagger UI (`/swagger-ui.html`; disabled in production by default).

---

Previous: [Admin Console](../guide/admin/admin.md) ｜ Next: [API Keys & Authentication](authentication.md)
