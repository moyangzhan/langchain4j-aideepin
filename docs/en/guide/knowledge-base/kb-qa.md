# Knowledge Base Q&A

> [← User Guide](../index.md) · [简体中文](../../../cn/guide/knowledge-base/kb-qa.md)

Click **Knowledge Base** in the left menu to enter the Q&A view: the knowledge base list on the left, the Q&A area on the right. This is the entry for asking a knowledge base directly; you can also [link the base to a character](../chat/character-config.md#linked-knowledge-bases) and use it in chat.

## Picking a Knowledge Base

Two tabs in the sidebar:

| Tab | Content |
|---|---|
| Mine | Bases you created (including private) |
| Public | Site-wide public bases (cloud icon = public, lock = private) |

Click the **i** button on an entry for details: document count, vector count, visibility, strict/lenient mode, max retrieval count, min score, and **Clear history** (wipes Q&A records of this base).

The **Knowledge base management** button at the bottom of the sidebar opens [My knowledge bases](manage.md).

## Asking

1. Pick an answering model in the top selector;
2. Type and send; the answer streams in;
   - In **strict mode**, no relevant hits return "[No answer]";
3. **Stop** interrupts generation;
4. Scroll up to page through history ("No Q&A records yet" when empty).

## Citation Tracing

Under the answer:

- **Citations**: hit segments displayed per segmentation mode (Q&A → question+answer; parent-child → child+parent; plain → segment text);
- **Graph**: [knowledge graph](graph.md) fragments referenced by the answer.

## Deleting Records

Deleting a Q&A record asks for confirmation ("The question and its answers will be deleted together. Continue?").

## API

**More → API** in the top bar generates this base's API key and endpoint docs to integrate Q&A into your program — see [API Reference · Knowledge Base Q&A](../../api/knowledge-base.md).

---

Previous: [Knowledge Graph](graph.md) ｜ Next: [Apps & Workflows](../workflow/workflow.md)
