# Workflow

> [← API Reference](index.md) · [简体中文](../../cn/api/workflow.md)

Trigger a workflow run from external systems with the resource-level API key bound to that workflow (see [API Keys & Authentication](authentication.md)).

## Request

```http
POST /ext/v1/workflow/run
```

| Parameter | Type | Required | Description |
|---|---|---|---|
| `inputs` | array | no | Input variables of the start node, as key-value pairs; defaults to empty |
| `response_mode` | string | no | `streaming` (default) / `blocking` |

`inputs` maps to the variables defined on the **start node** (text / number / file / boolean…); missing required variables fail validation.

## Response

- **streaming (default)**: SSE stream of the run process and output;
- **blocking**: waits for completion and returns the JSON result.

## Example

```bash
curl -X POST "http://<host>:9999/ext/v1/workflow/run" \
  -H "Authorization: <workflow API key>" \
  -H "Content-Type: application/json" \
  -d '{"inputs": [{"key": "text", "value": "Hello world"}], "response_mode": "blocking"}'
```

## Notes

- Each call creates a request record, inspectable via the app's **Requests** list;
- Flows with a **human interaction** node pause for input: API-triggered runs must be resumed from the requests list in the UI;
- Editing workflows and node details: see [Apps & Workflows](../guide/workflow/workflow.md).

---

Previous: [MCP Services](mcp.md) · Next: [Developer Docs](../dev/index.md)
