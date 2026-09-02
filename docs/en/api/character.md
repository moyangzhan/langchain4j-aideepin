# Character Chat

> [← API Reference](index.md) · [简体中文](../../cn/api/character.md)

Chat as a **character**. Use the resource-level API key bound to that character (see [API Keys & Authentication](authentication.md)); the character's persona, linked knowledge bases and MCP tools all apply.

## Request

```http
POST /ext/v1/character
```

| Parameter | Type | Required | Description |
|---|---|---|---|
| `query` | string | yes | The user question |
| `model` | string | no | Model name; defaults to the system default. `A_MODEL_NOT_AVAILABLE` if missing or disabled |
| `response_mode` | string | no | `streaming` (default) / `blocking` |

## Response

### streaming (default)

An SSE text stream, mirroring the in-page chat.

```bash
curl -N -X POST "http://<host>:9999/ext/v1/character" \
  -H "Authorization: <character API key>" \
  -H "Content-Type: application/json" \
  -d '{"query": "Introduce yourself in one sentence"}'
```

### blocking

Waits for completion and returns the JSON result at once.

```bash
curl -X POST "http://<host>:9999/ext/v1/character" \
  -H "Authorization: <character API key>" \
  -H "Content-Type: application/json" \
  -d '{"query": "Introduce yourself in one sentence", "response_mode": "blocking"}'
```

## Errors

| Error | Scenario |
|---|---|
| `401` | Invalid key (see [authentication](authentication.md#errors)) |
| `A_DATA_NOT_FOUND` | Character not found |
| `A_MODEL_NOT_AVAILABLE` | Model missing or disabled |

---

Previous: [API Keys & Authentication](authentication.md) · Next: [Knowledge Base Q&A](knowledge-base.md)
