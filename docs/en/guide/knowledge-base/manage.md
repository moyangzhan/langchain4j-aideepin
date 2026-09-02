# Create & Configure a Knowledge Base

> [← User Guide](../index.md) · [简体中文](../../../cn/guide/knowledge-base/manage.md)

Open **Knowledge Base → Knowledge base management** — the "My knowledge bases" overview. The table lists **title (link to detail), description, public flag, strict flag**; the actions column offers **Docs / API / Edit / Delete**.

- The search box filters by title (Enter or click the search button);
- The table paginates at the bottom ("{n} items in total").

> 📷 Screenshot TODO: the "My knowledge bases" list page. Replace with `![My knowledge bases](../../../image/en/guide/knowledge-base/manage-01.png)` once added.

## Creating a Knowledge Base

1. Click **Add** to open the form (five settings cards);
2. Fill in each group as described below;
3. Save and return to the list.

> [!TIP]
> For a first base, only the **title** is required — start importing with defaults; splitting and retrieval settings can be changed later (splitting changes trigger a rebuild, see below).

### 1. Basics

| Field | Required | Description |
|---|---|---|
| Title | yes | Name of the knowledge base |
| Description | no | Purpose, visible to others when public |
| Public | no, default private | Public: visible and usable by everyone; private: creator only |
| Strict mode | no, default lenient | In-app hint (translated): "Strict mode: strictly match the knowledge base; if there is no search result, return 'no answer' directly. Lenient mode: if there is no search result, pass the user's question to the LLM to continue." See [Overview](overview.md#strict-vs-lenient-mode) |

### 2. Document Index Settings (Vector)

| Field | Description |
|---|---|
| Split overlap | Characters shared by adjacent segments (marked "applies to new indexing after change") — keeps boundary information intact |
| Split strategy | Recursive / by paragraph / by line / by sentence / custom separator (required when custom; presets `\n\n (paragraph)`, `\n (newline)`, free input allowed) |
| Max tokens per segment | Upper bound of a segment |
| Token estimator | OpenAI / Qwen / Huggingface counting conventions — match your embedding model family |

> [!TIP]
> "By paragraph" suits well-structured documents; "recursive" is the general default; tables and logs suit "by line" or a custom separator.

### 3. Document Index Settings (Model)

**Model**: the in-app hint (translated) reads —

> The model used for graph extraction and QA generation; if empty, the first available model is used.

> [!WARNING]
> Graph extraction and QA generation **both consume this model's tokens**. Pick a cost-effective model; leaving it empty falls back to the first available model.

### 4. Retrieval Settings

| Field | Description |
|---|---|
| Max retrieved | Maximum segments recalled per query — higher is more thorough but costlier |
| Min score | Segments below this similarity are excluded — higher is stricter, less noise |

### 5. LLM Parameters

| Field | Description |
|---|---|
| System prompt | Persona for knowledge base Q&A (e.g. "You are the company policy assistant; answer only from retrieved content") |
| Creativity / randomness | Answer temperature: lower = conservative, higher = divergent |

> 📷 Screenshot TODO: the five settings cards of the create form (scrolling capture). Replace with `![Create form](../../../image/en/guide/knowledge-base/manage-02.png)` once added.

## Editing

Click **Edit** in the actions column. Note the consequences:

| Change | Effect |
|---|---|
| Retrieval settings, LLM parameters | Immediate, no rebuild |
| Splitting parameters (overlap / strategy / max tokens…) | Save prompts "Changing split parameters will automatically rebuild the vector indexes of all documents in this knowledge base. Save?" — confirming triggers an **automatic rebuild** of all indexes |

## API

The **API** action generates this base's API key and endpoint docs — see [API Reference · Knowledge Base Q&A](../../api/knowledge-base.md).

## Deleting

1. Click **Delete** in the actions column;
2. Confirm "The data cannot be recovered after deletion. Delete knowledge base [{title}]?".

> [!WARNING]
> Deleting removes documents, segments, vectors and graphs — **irrecoverable**. Characters linked to the base lose its retrieval.

---

Previous: [Overview](overview.md) ｜ Next: [Import Documents](document.md)
