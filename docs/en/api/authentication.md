# API Keys & Authentication

> [← API Reference](index.md) · [简体中文](../../cn/api/authentication.md)

## Two Key Types

The open API uses two key levels, both starting with `ext-`:

| Key type | Bound to | Endpoints | Count |
|---|---|---|---|
| **Resource-level** | A single character / knowledge base / workflow | That resource's endpoint | One per resource; regenerating **overwrites the old key** |
| **User-level** | Your account | Draw (draw), MCP | One per type |

Resource-level keys already carry the resource identity: **do not** (and cannot) pass a resource ID — the server resolves it from the key.

## Obtaining a Key

Fully self-service in the user web:

| Resource | Entry |
|---|---|
| Character | Chat top bar More → **API** |
| Knowledge base | KB Q&A page More → **API**; the **API** action in My knowledge bases |
| Workflow | App page top bar More → **API** |
| Draw | Draw page top bar More → **API** |
| MCP | Tools page top bar More → **API** |

In the dialog you can generate / regenerate the key, view the mask (`ext-a3f8****f6g7`) and **reveal the plaintext** (returned only on generation or reveal — store it safely). Only the resource owner and admins manage keys.

## Authentication

Send the key **verbatim in the `Authorization` header, with no `Bearer` prefix**:

```http
Authorization: ext-xxxxxxxxxxxxxxxx
```

Open API calls do not use the login token; their auth is independent from other endpoints.

## Errors

| Status | Scenario |
|---|---|
| `401` | Missing key, invalid format (no `ext-` prefix), nonexistent or revoked (no response body) |

Missing resources or permissions return business errors (e.g. `A_DATA_NOT_FOUND`); unavailable models return `A_MODEL_NOT_AVAILABLE`.

---

Previous: [API Reference](index.md) ｜ Next: [Character Chat](character.md)
