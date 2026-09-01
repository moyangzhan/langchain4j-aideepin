# Draw Tasks

> [← API Reference](index.md) · [简体中文](../../cn/api/draw.md)

Create and query text-to-image tasks with the **user-level draw key** (see [API Keys & Authentication](authentication.md)). Tasks run asynchronously: get the `uuid`, then poll the detail until done.

## Create a Draw Task

```http
POST /ext/v1/draw/generation
```

| Parameter | Type | Required | Description |
|---|---|---|---|
| `prompt` | string | yes | The prompt |
| `negativePrompt` | string | no | Negative prompt |
| `size` | string | no | Image size (model-dependent, e.g. `1024x1024`) |
| `quality` | string | no | Quality (where supported) |
| `number` | int | no | Number of images, ≥ 1, default 1 |
| `model` | string | no | Image model name |
| `seed` | int | no | Random seed |

Response:

```json
{ "uuid": "<task uuid>" }
```

## Get a Draw Task

```http
GET /ext/v1/draw/{uuid}
```

Only tasks owned by the key's owner are visible; otherwise `A_DRAW_NOT_FOUND`.

### Response Fields (DrawDto)

| Field | Description |
|---|---|
| `uuid` | Task ID |
| `prompt` | The prompt |
| `aiModelName` / `aiModelPlatform` | Model and platform used |
| `interactingMethod` | Generation method |
| `isPublic` | Public flag |
| `starCount` / `isStar` | Like count and whether the current user liked it |
| `processStatus` | Processing status (poll this for completion) |
| `processStatusRemark` | Status remark (failure reason etc.) |
| `imageUuids` / `imageUrls` | Generated image IDs and URLs |
| `duration` | Elapsed time |
| `dynamicParams` | Model dynamic parameters |
| `originalImageUuid/Url`, `maskImageUuid/Url` | Original / guidance images (background generation etc.) |
| `userUuid` / `userName` / `createTime` | Ownership and time |

## Example

```bash
# Create the task
curl -X POST "http://<host>:9999/ext/v1/draw/generation" \
  -H "Authorization: <draw user-level key>" \
  -H "Content-Type: application/json" \
  -d '{"prompt": "A white owl under moonlight, ink painting style", "number": 2}'

# Poll the detail; take images from imageUrls once processStatus is done
curl "http://<host>:9999/ext/v1/draw/<task uuid>" \
  -H "Authorization: <draw user-level key>"
```

---

Previous: [Knowledge Base Q&A](knowledge-base.md) ｜ Next: [MCP Services](mcp.md)
