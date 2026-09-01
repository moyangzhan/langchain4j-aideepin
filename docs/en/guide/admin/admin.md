# Admin Console

> [← User Guide](../index.md) · [简体中文](../../../cn/guide/admin/admin.md)

The admin console (admin-web) is for system administrators and is deployed separately from the user web. Sign in with an admin account.

## User Management

- Search / view user details;
- **Add users** and **activate on behalf** (the fix when activation emails go missing);
- **Freeze / unfreeze** accounts; edit profiles.

## Model Platforms & Models

The admin's most-used feature — "empty model list" or "model unavailable" for users is usually solved here:

| Action | Description |
|---|---|
| Model platforms | Maintain platforms (OpenAI, DeepSeek, DashScope, SiliconFlow, Ollama…): **platform API keys are configured here** |
| Models | Add / edit / delete models; toggle `is_enable` |
| Health status | View per-model health and reasons; run a manual **full probe** |

Integrating a brand-new platform (database config + optional code) is covered in the [Model Platform Integration Guide](../../dev/model-platform-integration.md).

## Token Monitoring

Paged LLM call records: user, platform, model, **input / output tokens**, duration and time — filterable by user, model, platform, source and time range.

## Statistics

A dashboard summarizing users, knowledge bases, token consumption, characters and image consumption.

## Content & Resources

| Module | Description |
|---|---|
| Character management | View and manage all users' characters |
| Preset characters | Maintain the user-side preset library |
| Knowledge base management | View and manage all bases |
| MCP management | Register / maintain MCP services (users can only use them) |
| Workflow management | Search and toggle workflows |
| Workflow components | Maintain the node components available on the canvas |

## System Configuration

Key-value settings including **ASR / TTS** (the globally unique speech services: model, platform, duration and size limits), quotas and rate limits. Changes take effect immediately or on restart as noted.

---

Previous: [Prompt Store](../prompt-store.md) ｜ Next: [API Reference](../../api/index.md)
