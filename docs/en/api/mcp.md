# MCP Services

> [← API Reference](index.md) · [简体中文](../../cn/api/mcp.md)

Fetch the MCP service list with the **user-level mcp key** (see [API Keys & Authentication](authentication.md)) — useful when you want the same tools in your own client.

## All Supported Services

```http
GET /ext/v1/mcp/supported
```

## Services Enabled by the Current User

```http
GET /ext/v1/mcp/active
```

Returns the services the key's owner enabled on the Tools page; identical to `supported` plus an `isEnable` field.

## Response Fields (McpPublicInfo)

| Field | Description |
|---|---|
| `uuid` | Service ID |
| `title` | Service name |
| `transportType` | Transport: `sse` / `streamable_http` / `stdio` |
| `sseUrl` | Address of SSE / HTTP services |
| `sseTimeout` | Timeout |
| `stdioCommand` | Launch command of stdio services |
| `remark` | Description |
| `installType` | Install type |

## Example

```bash
curl "http://<host>:9999/ext/v1/mcp/supported" \
  -H "Authorization: <mcp user-level key>"
```

The list reflects what the admin registered; services cannot be added via this API.

---

Previous: [Draw Tasks](draw.md) · Next: [Workflow](workflow.md)
