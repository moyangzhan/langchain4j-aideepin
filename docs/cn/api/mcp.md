# MCP 服务

> [← API 参考](index.md) · [English](../../en/api/mcp.md)

用**用户级 mcp Key**（见 [API Key 与鉴权](authentication.md)）获取 MCP 服务清单，便于在你的客户端中接入同样的工具。

## 系统全部已启用的服务

```http
GET /ext/v1/mcp/supported
```

## 当前用户已启用的服务

```http
GET /ext/v1/mcp/active
```

返回该 Key 属主在工具页中启用的服务，字段比 supported 多一个 `isEnable`。

## 响应字段（McpPublicInfo）

| 字段 | 说明 |
|---|---|
| `uuid` | 服务 ID |
| `title` | 服务名称 |
| `transportType` | 传输方式：`sse` / `streamable_http` / `stdio` |
| `sseUrl` | SSE / HTTP 服务的地址 |
| `sseTimeout` | 超时时间 |
| `stdioCommand` | stdio 服务的启动命令 |
| `remark` | 说明 |
| `installType` | 安装类型 |

## 示例

```bash
curl "http://<host>:9999/ext/v1/mcp/supported" \
  -H "Authorization: <mcp用户级Key>"
```

服务清单内容取决于管理员在管理端登记的 MCP 服务，用户端无法通过 API 新增服务。

---

上一篇：[绘图任务](draw.md) ｜ 下一篇：[工作流](workflow.md)
