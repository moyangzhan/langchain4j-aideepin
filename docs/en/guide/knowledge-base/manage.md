# Create & Configure a Knowledge Base

> [← User Guide](../index.md) · [简体中文](../../../cn/guide/knowledge-base/manage.md)

Open **Knowledge Base → Knowledge base management** — the "My knowledge bases" overview. The table lists **title (link to detail), description, public flag, strict flag**; the actions column offers **Docs / API / Edit / Delete**.

## Creating

Click **Add** to open the form with five settings groups:

### 1. Basics

| Field | Description |
|---|---|
| Title (required) | Name of the knowledge base |
| Description | Purpose, visible to others when public |
| Public | Public: visible and usable by everyone; private: creator only |
| Strict mode | Strict: return "no answer" when nothing is retrieved; lenient: let the LLM answer freely. See [Overview](overview.md#strict-vs-lenient-mode) |

### 2. Document Index Settings (Vector)

| Field | Description |
|---|---|
| Split overlap | Characters shared by adjacent segments to preserve boundaries (applies to **new indexing** after change) |
| Split strategy | Recursive / by paragraph / by line / by sentence / custom separator (required when custom) |
| Max tokens per segment | Upper bound of a segment |
| Token estimator | OpenAI / Qwen / Huggingface counting conventions |

### 3. Document Index Settings (Model)

**Model**: the LLM used for graph extraction and AI Q&A generation — both consume this model's tokens.

### 4. Retrieval Settings

| Field | Description |
|---|---|
| Max retrieved | Maximum segments recalled per query |
| Min score | Segments below this similarity are excluded |

### 5. LLM Parameters

| Field | Description |
|---|---|
| System prompt | Persona for knowledge base Q&A |
| Creativity / randomness | Answer temperature |

## Save Prompt

Changing **splitting parameters** (overlap, strategy, max tokens…) prompts: "Changing split parameters will automatically rebuild the vector indexes of all documents in this knowledge base. Save?" — confirm and the rebuild runs automatically.

## Search & Delete

- The search box filters by title; the table paginates;
- **Delete** asks "The data cannot be recovered after deletion. Delete knowledge base [{title}]?" — documents, segments, vectors and graph are removed together.

---

Previous: [Overview](overview.md) ｜ Next: [Import Documents](document.md)
