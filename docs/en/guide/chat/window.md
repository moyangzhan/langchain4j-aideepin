# Chat Window

> [← User Guide](../index.md) · [简体中文](../../../cn/guide/chat/window.md)

The chat window has three parts: the **character list** on the left, the **message area** in the middle, and the **input area** at the bottom. Each character keeps its own history — switching characters switches the conversation.

## Sending Messages & Streaming Answers

1. Type a question and press Enter or click send.
2. Answers stream in. Status hints appear during generation: **Analyzing question → Searching knowledge base (when linked) → Reasoning → Answering**.
3. Click **Stop** to interrupt generation.

A **scroll to bottom** button appears at the bottom of the message area for long conversations.

### Deep Thinking

- Toggle **Deep thinking** on to get the reasoning process before the final answer.
- Some models (e.g. deepseek-reasoner) always think and cannot be turned off; models without support disable the toggle with a hint.
- A per-character default can be set in [Character Settings](character-config.md).

### Web Search

With **Web search** on, answers reference search engine results. Note: DeepSeek's deep thinking is mutually exclusive with tools/web search — enabling both auto-disables one.

### Multiple Answers

After regenerating, multiple answers to the same question appear as "Answer 1 / Answer 2 …" tabs for comparison.

## Message Actions

- **Copy / Copy code**: copy the answer or a code block;
- **Delete**: deleting a question also deletes its answers (confirmation required);
- **Regenerate**: re-run the question.

## Uploading Images (Vision)

- Only **vision (multimodal) models** accept images; the button is unavailable for text-only models;
- PNG / JPG, up to 4MB each;
- The image is sent with your question and the AI answers about its content.

## Knowledge Base & Tool Tags in the Input Bar

- **Knowledge base**: click the tag to adjust the character's linked knowledge bases on the fly;
- **Tools**: click the tag to configure the character's MCP tools on the fly.

Both are equivalent to the settings in [Character Settings](character-config.md) and only affect the current character.

## Context Mode

The **context mode** icon controls whether history is sent:

- Carry (default): multi-turn conversation, the AI remembers;
- Don't carry: each question stands alone.

## Memory & Citations

Three buttons under each answer provide traceability:

| Button | Content |
|---|---|
| Memory | Long-term memories hit by this answer, grouped into **semantic** / **episodic** |
| Citations | Hit knowledge base segments, displayed per segmentation mode: Q&A mode shows the hit question and answer; parent-child mode shows the hit child chunk and its parent; plain mode shows the segment text |
| Graph | Knowledge graph fragments referenced by the answer |

## Top Bar Actions

- **Edit**: open the current character's edit form;
- **API**: view/manage this character's open API key and endpoint docs — see [API Reference · Character Chat](../../api/character.md).

---

Previous: [Quick Start](../quick-start.md) ｜ Next: [Characters & Presets](character.md)
