# API 参考

> [← 返回总目录](../index.md) · [English](../../en/api/index.md)

AIDeepIn 开放 API（`/ext/v1`）让你把平台的 AI 能力接入自己的程序：**角色对话、知识库问答、绘图任务、MCP 服务清单、工作流运行**。所有接口均为自助使用——在页面上生成 API Key 即可调用，无需审批。

## 端点总览

| 能力 | 端点 | 方法 | Key 类型 |
|---|---|---|---|
| 角色对话 | `/ext/v1/character` | POST | 资源级（绑定角色） |
| 知识库问答 | `/ext/v1/knowledge` | POST | 资源级（绑定知识库） |
| 创建绘图任务 | `/ext/v1/draw/generation` | POST | 用户级 |
| 查询绘图任务 | `/ext/v1/draw/{uuid}` | GET | 用户级 |
| 已支持的 MCP 服务 | `/ext/v1/mcp/supported` | GET | 用户级 |
| 已启用的 MCP 服务 | `/ext/v1/mcp/active` | GET | 用户级 |
| 运行工作流 | `/ext/v1/workflow/run` | POST | 资源级（绑定工作流） |

## 快速示例

```bash
curl -N -X POST "http://<host>:9999/ext/v1/character" \
  -H "Authorization: <你的API Key>" \
  -H "Content-Type: application/json" \
  -d '{"query": "你好，介绍一下你自己", "response_mode": "streaming"}'
```

开始之前请先阅读 [API Key 与鉴权](authentication.md)。

## 接口列表

1. [API Key 与鉴权](authentication.md)：两种 Key、获取方式、鉴权头与错误码
2. [角色对话](character.md)：以某个角色身份发起对话（流式 / 阻塞）
3. [知识库问答](knowledge-base.md)：对某个知识库提问
4. [绘图任务](draw.md)：创建与查询文生图任务
5. [MCP 服务](mcp.md)：获取服务清单
6. [工作流](workflow.md)：触发工作流运行

> 服务端接口的完整机器可读定义可访问 Swagger UI（`/swagger-ui.html`，生产环境默认关闭）。

---

上一篇：[管理端使用](../guide/admin/admin.md) ｜ 下一篇：[API Key 与鉴权](authentication.md)
