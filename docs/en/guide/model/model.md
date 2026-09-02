# Model Selection

> [← User Guide](../index.md) · [简体中文](../../../cn/guide/model/model.md)

Models in AIDeepIn are configured by the admin (platform API keys and enablement live in the admin console); regular users only **pick a model** where they use it. The picker is **global**: chat, knowledge base Q&A and workflows share the same selection.

## Where to Pick

| Scenario | Location |
|---|---|
| Text models (chat / KB Q&A / workflows) | The model selector above the chat input or on the KB Q&A page |
| Image models | The image model selector on the Draw page |

The selector shows "platform avatar + model name".

## Model Types

| Type | Purpose | Entry |
|---|---|---|
| Text | Chat, Q&A, workflow generation | Model selectors |
| Vision (multimodal) | Text plus image recognition | Upload-image button appears in chat |
| Image (text-to-image) | Generate images from text | Draw page |
| TTS | AI reply voices | Character [voice settings](../chat/character-config.md) |
| ASR | Voice questions to text | Mic icon in the chat input bar |

## Capability Differences

The UI adapts automatically per model:

- **Deep thinking**:
  - Unsupported models disable the toggle ("The model does not support deep thinking");
  - Models that can't turn it off (e.g. deepseek-reasoner) keep it on ("The model does not support disabling deep thinking");
- **Web search**: unsupported models show "The model does not support web search";
- **Mutual exclusion**: DeepSeek's deep thinking vs tools/web search — enabling both auto-disables one with a hint;
- **Image recognition**: only vision models show the upload entry (PNG / JPG, ≤ 4MB; otherwise "The model does not support image recognition").

## When a Model Is Unavailable

- Models with a **red dot** failed health checks: hover shows the specific reason (healthReason) or "Unavailable";
- Pick an available model. Enablement and platform configuration are admin tasks, see [Admin Console](../admin/admin.md#model-platforms--models); integrating a brand-new platform is covered in the [Model Platform Integration Guide](../../dev/model-platform-integration.md).

> [!TIP]
> Models differ a lot on the same task: prefer reasoner models for reasoning-heavy work; everyday chat is cheaper on regular text models. The free/paid flag affects how your [quota](../account/usage.md) is counted.

---

Previous: [Services & Tools](../mcp/mcp.md) ｜ Next: [Sign-up, Login & Settings](../account/account.md)
