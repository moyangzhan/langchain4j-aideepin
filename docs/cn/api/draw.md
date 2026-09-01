# 绘图任务

> [← API 参考](index.md) · [English](../../en/api/draw.md)

用**用户级 draw Key**（见 [API Key 与鉴权](authentication.md)）创建与查询文生图任务。任务异步执行：先拿到任务 `uuid`，再轮询详情直到完成。

## 创建绘图任务

```http
POST /ext/v1/draw/generation
```

| 参数 | 类型 | 必填 | 说明 |
|---|---|---|---|
| `prompt` | string | 是 | 提示词 |
| `negativePrompt` | string | 否 | 负向提示词 |
| `size` | string | 否 | 图片尺寸（取决于模型，如 `1024x1024`） |
| `quality` | string | 否 | 图片质量（支持的模型适用） |
| `number` | int | 否 | 生成张数，≥ 1，默认 1 |
| `model` | string | 否 | 图片模型名 |
| `seed` | int | 否 | 随机种子 |

响应：

```json
{ "uuid": "<任务uuid>" }
```

## 查询绘图任务

```http
GET /ext/v1/draw/{uuid}
```

只能查询**属于 Key 属主**的任务，否则返回 `A_DRAW_NOT_FOUND`。

### 响应字段（DrawDto）

| 字段 | 说明 |
|---|---|
| `uuid` | 任务 ID |
| `prompt` | 提示词 |
| `aiModelName` / `aiModelPlatform` | 所用模型与平台 |
| `interactingMethod` | 生成方式 |
| `isPublic` | 是否公开 |
| `starCount` / `isStar` | 点赞数与当前用户是否点赞 |
| `processStatus` | 处理状态（据此判断是否完成） |
| `processStatusRemark` | 状态说明（失败原因等） |
| `imageUuids` / `imageUrls` | 生成图片的 ID 与访问地址 |
| `duration` | 耗时 |
| `dynamicParams` | 模型动态参数 |
| `originalImageUuid/Url`、`maskImageUuid/Url` | 原图 / 引导图（背景生成等场景） |
| `userUuid` / `userName` / `createTime` | 归属与时间 |

## 示例

```bash
# 创建任务
curl -X POST "http://<host>:9999/ext/v1/draw/generation" \
  -H "Authorization: <draw用户级Key>" \
  -H "Content-Type: application/json" \
  -d '{"prompt": "一只在月光下的白色猫头鹰，水墨风", "number": 2}'

# 轮询详情，processStatus 完成后从 imageUrls 取图
curl "http://<host>:9999/ext/v1/draw/<任务uuid>" \
  -H "Authorization: <draw用户级Key>"
```

---

上一篇：[知识库问答](knowledge-base.md) ｜ 下一篇：[MCP 服务](mcp.md)
