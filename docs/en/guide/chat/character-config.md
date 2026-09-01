# Character Settings

> [← User Guide](../index.md) · [简体中文](../../../cn/guide/chat/character-config.md)

Besides basics, the character form (shared by create and edit) supports linking knowledge bases, MCP tools and voice behavior.

## Basics

| Field | Description | Example |
|---|---|---|
| Name | Character name | Li Bai |
| Remark | Note for yourself | A veteran poet |
| Persona | System prompt defining the AI's role and behavior | You are Li Bai from the Tang dynasty; answer poetically |

## Deep Thinking

Sets the character's default deep-thinking switch. Some models (e.g. deepseek-reasoner) cannot disable it; models without support ignore it.

## Linked Knowledge Bases

- Linked knowledge bases appear as removable tags;
- Click **Add more knowledge bases** to open the picker: search by title; the table lists **title / description / attributes** (mine or the creator's name, public or private);
- After saving, chats with this character retrieve from the knowledge bases first; see [citations](window.md#memory--citations) under answers.

Creating and importing knowledge bases is covered in the [Knowledge Base](../knowledge-base/overview.md) chapter.

## Services & Tools (MCP)

Check the MCP tools the character may call. **Enable more AI tools** jumps to the Tools page — see [Services & Tools](../mcp/mcp.md).

## AI Reply Format

| Option | Behavior |
|---|---|
| Auto | Follows your input format: text in → text out, voice in → voice out |
| Text | Always reply as text |
| Voice | Always reply as voice |

Related options:

- **Auto-play AI voice replies**: play the audio when the answer arrives;
- **Voice**: pick from the listed server-side TTS voices; with browser-side synthesis it says "the current voice comes from the browser, no need to specify".

Full voice details in [Voice Input & Playback](voice.md).

## Save & Delete

- **Save** at the bottom submits changes;
- **Delete** removes the character (confirmation required).

---

Previous: [Characters & Presets](character.md) ｜ Next: [Voice Input & Playback](voice.md)
