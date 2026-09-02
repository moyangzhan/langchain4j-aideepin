# Quick Start

> [← User Guide](index.md) · [简体中文](../../cn/guide/quick-start.md)

This page walks the shortest path: **sign up & activate → get to know the UI → first conversation**. Model platforms are pre-configured by the admin, so regular users can start right away — about 5 minutes end to end.

> 📷 Screenshot TODO: the login dialog (Login / Register tabs). Replace with `![Login dialog](../../image/en/guide/quick-start-01.png)` once added.

## 1. Sign Up & Activate

1. Open the AIDeepIn site, click **Login** at the bottom-left, and the login window pops up.
2. Switch to the **Register** tab.
3. Fill in:
   - **Email**: your login account and the address receiving the activation mail;
   - **Password**: at least 6 characters;
   - **Confirm password**: type the password again ("The two passwords do not match" if different);
   - **Captcha**: the 4 uppercase letters/digits in the image — click the image to refresh.
4. Click **Register**. On success the system tells you to check for the activation email.

> [!NOTE]
> If the mail doesn't arrive, check the spam folder first; if it's still missing, ask the admin to **activate you from the console** (see [Admin Console](admin/admin.md#user-management)).

5. Open the email and click the activation link.
6. The result page shows **activation successful** with a 5-second countdown back to the home page; on failure the reason is shown (most often an expired link).

> [!TIP]
> Once activated, log in with your email and password. Password reset and other account tasks: [Sign-up, Login & Settings](account/account.md).

## 2. Get to Know the UI

After logging in, the global menu sits on the far left (icons only, hover for labels), top to bottom:

| Menu | Function |
|---|---|
| Chat | Talk to AI characters |
| Draw | Text to image |
| Gallery | Browse public images and your likes |
| Knowledge Base | Q&A grounded on knowledge bases |
| Apps | Create and run workflows |
| Tools | Enable MCP extensions |

Three fixed entries at the bottom-left:

| Entry | Function |
|---|---|
| Prompt Store | Manage reusable prompts (see [Prompt Store](prompt-store.md)) |
| Settings | Theme, language, quota, change password (see [Quota & Usage](account/usage.md)) |
| Login / Log out | **Login** when signed out; log out from Settings when signed in |

> 📷 Screenshot TODO: the home screen with the six menu items and bottom-left entries annotated. Replace with `![Home screen](../../image/en/guide/quick-start-02.png)` once added.

## 3. First Conversation

1. Click **Chat**. New accounts get a default character automatically — no setup needed.
2. Pick a model in the selector above the input box:
   - Models are configured by the admin and shown as "platform avatar + model name";
   - A greyed-out model is currently unavailable — just pick another;
   - The selection is **global**: drawing and knowledge base Q&A follow it too.
3. Type a question (e.g. "Introduce yourself in three sentences").
4. Press Enter or click send.
5. The answer streams in. Click **Stop** anytime to interrupt.

> 📷 Screenshot TODO: the chat window with the model selector, input box and send/stop buttons annotated. Replace with `![Chat window](../../image/en/guide/quick-start-03.png)` once added.

### Try these next

- **Deep thinking**: toggle it on to see the reasoning process before the answer (some reasoner models can't turn it off — see [Model Selection](model/model.md#capability-differences)).
- **Web search**: answers reference search results.
- **Upload an image**: with a vision (multimodal) model selected, upload a PNG/JPG (≤ 4MB) for recognition.
- **Switch characters**: click **New Character** above the list, or create one from 11 **preset** categories — see [Characters & Presets](chat/character.md).
- **Voice**: click the mic icon to record; the audio is transcribed and sent — see [Voice Input & Playback](chat/voice.md).
- **Ground answers in your documents**: create a knowledge base — see [Knowledge Base Overview](knowledge-base/overview.md).

## 4. Next Steps

- Ground answers in your own documents: read the [Knowledge Base](knowledge-base/overview.md) chapter.
- Build automations: read [Apps & Workflows](workflow/workflow.md).
- Integrate chat into your own program: read the [API Reference](../api/index.md).
- Check your usage: read [Quota & Usage](account/usage.md).

---

Previous: [User Guide contents](index.md) · Next: [Chat Window](chat/window.md)
