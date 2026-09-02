# Knowledge Base Overview

> [← User Guide](../index.md) · [简体中文](../../../cn/guide/knowledge-base/overview.md)

A knowledge base (RAG) hands your own material to the AI: imported documents are split into **segments** and **indexed**; when you ask, the system retrieves the most relevant segments and injects them into the model, grounding answers with traceable **citations**.

Good fits: policy Q&A, product-manual support, personal notes retrieval, domain assistants.

## Overall Flow

```
Create knowledge base → Import documents (pick segmentation mode) → Index (vectorize / graph) → Q&A or link to a character
```

| Step | What | See |
|---|---|---|
| 1. Create | Retrieval and splitting settings | [Create & Configure](manage.md) |
| 2. Import | Form entry or file upload, choose segmentation mode | [Import Documents](document.md) |
| 3. Index | Vectorization (required) and graph extraction (optional) | [Import Documents](document.md#indexing) |
| 4. Maintain | Segment enable/disable, QA upkeep, retries | [Segment Management](segment.md), [Q&A Import & Generation](qa-import.md) |
| 5. Use | Knowledge base Q&A, or link the base to a character | [Knowledge Base Q&A](kb-qa.md), [Character Settings](../chat/character-config.md) |

> 📷 Screenshot TODO: the KB Q&A page and the "My knowledge bases" page. Replace with `![KB entries](../../../image/en/guide/knowledge-base/overview-01.png)` once added.

## Two Entry Points

- **Knowledge base Q&A**: the **Knowledge Base** menu item — pick a base and ask;
- **Knowledge base management**: the **Knowledge base management** button at the bottom of the Q&A sidebar — the "My knowledge bases" page for creating and maintaining.

## Three Segmentation Modes

The segmentation mode decides how documents are chunked and retrieved. It is a **document-level property** — chosen when importing or editing a document; one knowledge base can mix modes.

| Mode | Best for | Retrieval |
|---|---|---|
| **Plain segmentation** | Continuous text: manuals, articles | Auto-chunks per the base's splitting strategy; semantic similarity recall |
| **Q&A** | Paired content: FAQs, support scripts | Question-answer pairs; hitting any question recalls the answer |
| **Parent-child** | Long, hierarchical documents | Small chunks (children) match precisely; the hit returns its **parent** (larger context) to the model |

How to choose:

- Material is naturally question-answer → **Q&A** (highest accuracy, but requires reformatting);
- Continuous prose with clear paragraphs → **plain** (easiest);
- Long documents needing both precise hits and full context → **parent-child** (e.g. a manual: child hits a spec item, parent returns the section).

> [!NOTE]
> Q&A mode only imports XLSX / XLS / CSV files; parent-child mode has its own child max-token setting (default 200).

## Two Index Types

| Index | Purpose | Note |
|---|---|---|
| **Vector index** | Embeds segments for semantic similarity search | The foundation; prerequisite for Q&A |
| **Graph index** | Extracts entities and relations into a knowledge graph | Optional; extraction uses an LLM and **consumes tokens** |

Both can coexist; answers can be traced via citations and the citation graph (see [Knowledge Graph](graph.md)). They complement each other: vectors excel at "semantically similar", graphs at "multi-hop relations" (who is the head of A's department).

## Strict vs Lenient Mode

A knowledge-base-level answering strategy (set at creation, see [Create & Configure](manage.md)):

- **Strict**: with no relevant hits, returns "[No answer]" instead of making things up — for policies and compliance;
- **Lenient**: with no hits, the question goes to the LLM for a free-form answer — for assistant scenarios.

## Public vs Private

- **Private**: visible and usable only by the creator;
- **Public**: every user can see and use it (read-only) under the "Public" tab.

## Where to Go Next

| Goal | Page |
|---|---|
| Build a base from scratch | [Create & Configure](manage.md) |
| Import Word / PDF | [Import Documents](document.md) |
| Already have an FAQ sheet | [Q&A Import & Generation](qa-import.md) |
| Keep one segment out of retrieval | [Segment Management](segment.md) |
| Try graph retrieval | [Knowledge Graph](graph.md) |
| Ask the base directly | [Knowledge Base Q&A](kb-qa.md) |

---

Previous: [Gallery](../draw/gallery.md) ｜ Next: [Create & Configure](manage.md)
