# 工作流

> [← API 参考](index.md) · [English](../../en/api/workflow.md)

用绑定该工作流的资源级 API Key（见 [API Key 与鉴权](authentication.md)）从外部系统触发一次工作流运行。

## 请求

```http
POST /ext/v1/workflow/run
```

| 参数 | 类型 | 必填 | 说明 |
|---|---|---|---|
| `inputs` | array | 否 | 工作流开始节点定义的输入变量，键值对数组；缺省为空 |
| `response_mode` | string | 否 | `streaming`（默认）/ `blocking` |

`inputs` 与工作流**开始节点**中定义的变量（文本 / 数字 / 文件 / 布尔等）对应，未提供的必填变量会导致运行校验失败。

## 响应

- **streaming（默认）**：SSE 流式返回运行过程与输出；
- **blocking**：等待运行完成后一次性返回 JSON 结果。

## 示例

```bash
curl -X POST "http://<host>:9999/ext/v1/workflow/run" \
  -H "Authorization: <工作流API Key>" \
  -H "Content-Type: application/json" \
  -d '{"inputs": [{"key": "text", "value": "Hello world"}], "response_mode": "blocking"}'
```

## 说明

- 每次调用产生一条请求记录，可在应用页的**请求列表**中查看执行详情；
- 含**人机交互**节点的工作流会暂停等待输入：API 触发的运行需到页面请求列表中补充输入后续跑；
- 工作流的编辑与节点说明见[应用与工作流](../guide/workflow/workflow.md)。

---

上一篇：[MCP 服务](mcp.md) ｜ 下一篇：[开发文档](../dev/index.md)
