# Knowledge Base Q&A

> [← API Reference](index.md) · [简体中文](../../cn/api/knowledge-base.md)

RAG Q&A against a **knowledge base**. Use the resource-level API key bound to that base (see [API Keys & Authentication](authentication.md)); strict/lenient mode and retrieval settings apply.

## Request

```http
POST /ext/v1/knowledge
```

| Parameter | Type | Required | Description |
|---|---|---|---|
| `query` | string | yes | The user question |
| `model` | string | no | Answering model; `A_MODEL_NOT_AVAILABLE` if missing or disabled |
| `response_mode` | string | no | `streaming` (default) / `blocking` |

## Response

### streaming (default)

An SSE text stream. In strict mode, when nothing relevant is retrieved the API returns **empty content** (the web UI displays it as "[No answer]") — the model does not make up an answer.

### blocking

Waits and returns the JSON result (including the answer text).

```bash
curl -X POST "http://<host>:9999/ext/v1/knowledge" \
  -H "Authorization: <knowledge base API key>" \
  -H "Content-Type: application/json" \
  -d '{"query": "What is the company vacation policy?", "response_mode": "blocking"}'
```

Each call creates a record in the base's Q&A history, viewable and clearable in the UI.

## Errors

| Error | Scenario |
|---|---|
| `401` | Invalid key |
| `A_DATA_NOT_FOUND` | Knowledge base not found |
| `A_MODEL_NOT_AVAILABLE` | Model missing or disabled |

---

Previous: [Character Chat](character.md) · Next: [Draw Tasks](draw.md)
