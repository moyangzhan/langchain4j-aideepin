# 角色对话

> [← API 参考](index.md) · [English](../../en/api/character.md)

以某个**角色**的身份发起对话。使用绑定该角色的资源级 API Key（见 [API Key 与鉴权](authentication.md)），角色的设定、关联的知识库与 MCP 工具均会生效。

## 请求

```http
POST /ext/v1/character
```

| 参数 | 类型 | 必填 | 说明 |
|---|---|---|---|
| `query` | string | 是 | 用户问题 |
| `model` | string | 否 | 模型名；缺省用系统默认。不存在或未启用返回 `A_MODEL_NOT_AVAILABLE` |
| `response_mode` | string | 否 | `streaming`（默认）/ `blocking` |

## 响应

### streaming（默认）

SSE 文本流，与页面对话一致地逐段输出回答。

```bash
curl -N -X POST "http://<host>:9999/ext/v1/character" \
  -H "Authorization: <角色API Key>" \
  -H "Content-Type: application/json" \
  -d '{"query": "用一句话介绍你自己"}'
```

### blocking

等待生成完成后一次性返回 JSON 结果。

```bash
curl -X POST "http://<host>:9999/ext/v1/character" \
  -H "Authorization: <角色API Key>" \
  -H "Content-Type: application/json" \
  -d '{"query": "用一句话介绍你自己", "response_mode": "blocking"}'
```

## 错误

| 错误 | 场景 |
|---|---|
| `401` | Key 无效（见[鉴权](authentication.md#错误)） |
| `A_DATA_NOT_FOUND` | 角色不存在 |
| `A_MODEL_NOT_AVAILABLE` | 指定模型不存在或未启用 |

---

上一篇：[API Key 与鉴权](authentication.md) ｜ 下一篇：[知识库问答](knowledge-base.md)
