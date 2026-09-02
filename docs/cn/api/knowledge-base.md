# 知识库问答

> [← API 参考](index.md) · [English](../../en/api/knowledge-base.md)

对某个**知识库**发起 RAG 问答。使用绑定该知识库的资源级 API Key（见 [API Key 与鉴权](authentication.md)），知识库的严格 / 宽松模式与召回参数均生效。

## 请求

```http
POST /ext/v1/knowledge
```

| 参数 | 类型 | 必填 | 说明 |
|---|---|---|---|
| `query` | string | 是 | 用户问题 |
| `model` | string | 否 | 回答模型名；不存在或未启用返回 `A_MODEL_NOT_AVAILABLE` |
| `response_mode` | string | 否 | `streaming`（默认）/ `blocking` |

## 响应

### streaming（默认）

SSE 文本流。严格模式下检索不到相关内容时返回**空内容**（页面上显示为「[无答案]」），不调用模型编造。

### blocking

等待完成后一次性返回 JSON 结果（含答案文本）。

```bash
curl -X POST "http://<host>:9999/ext/v1/knowledge" \
  -H "Authorization: <知识库API Key>" \
  -H "Content-Type: application/json" \
  -d '{"query": "公司的年假制度是怎样的？", "response_mode": "blocking"}'
```

每次调用都会在知识库问答历史中生成一条记录，可在页面中查看与清理。

## 错误

| 错误 | 场景 |
|---|---|
| `401` | Key 无效 |
| `A_DATA_NOT_FOUND` | 知识库不存在 |
| `A_MODEL_NOT_AVAILABLE` | 指定模型不存在或未启用 |

---

上一篇：[角色对话](character.md) ｜ 下一篇：[绘图任务](draw.md)
