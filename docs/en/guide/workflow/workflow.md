# Apps & Workflows

> [← User Guide](../index.md) · [简体中文](../../../cn/guide/workflow/workflow.md)

The **Apps** menu item is the workflow feature: visually orchestrate processing nodes into automated AI flows (translation, FAQ grooming, multi-step content generation), run them repeatedly, or trigger them via API.

## My Apps

- The sidebar has **Mine / Public** tabs; click **New App** and fill in title (required), remark and visibility;
- Click an entry to open it; hovering shows the edit entry;
- The edit dialog offers **Update**, **Save as new**, and **Delete** ("Inputs and outputs will be deleted together. Continue?").

Non-creators opening a public app get view-only mode — "only the workflow creator can save". Public apps can be **copied** via the more menu into your own editable copy.

## Layout

Top bar: app title, the **Workflow ↔ Requests** view switch, and the more menu (**Edit / View**, **Copy**, **API**).

- **Workflow view**: the canvas editor;
- **Requests view**: the run history of this app.

## Editing a Workflow

- The left **node panel** lists available nodes by category — drag onto the canvas;
- Select a node to edit it in the right **properties panel** (name and component parameters);
- **Only one start node** is allowed — it is the entry and defines input variables and the opening message;
- **Run** tries the current flow on the canvas; **Save** persists; **Re-layout** tidies node positions;
- Delete nodes via their own delete action.

### Available Nodes (17)

| Category | Node | Purpose |
|---|---|---|
| Basic | Start | Entry point: input variables (name / label / type [text, number, file, boolean] / required / multiple / max files) and opening message |
| | End | Exit point, assembles the output |
| | Generate answer | Call an LLM for the reply |
| | Human interaction | Pause for user input (with pre-input hint); resume later from the requests list |
| AI | OpenAI image | Prompt / size / quality / seed |
| | Wanxiang image | DashScope text-to-image |
| | Document extraction | Extract text from uploaded files |
| | Keyword extraction | Extract keywords (count configurable) |
| | FAQ extraction | Extract FAQ pairs (count, country/region, language) |
| | Classification | Route input by defined categories |
| | Agent | Autonomous node: role, abilities, tools, web search |
| Knowledge | Knowledge retrieval | Search a chosen base (query / top-k / min score / default reply) |
| Logic | Condition | Route by AND/OR conditions, with a fallback |
| | Template | Compose text from a template with `{var}`, `{input}` |
| Integration | HTTP request | Call external APIs: headers, params, timeout, retries, HTML stripping, output variable |
| | Email | SMTP server / port / from / to / cc / subject / body |

## Running & Requests

- **Run** on the canvas (or launching from the app page) creates a **request** record;
- The **requests list** pages through history; click for the **execution detail**: each node's inputs/outputs and file previews;
- Flows with a **human interaction** node pause with "Paused, waiting for user input…" — supply the input (files TXT / PDF etc., ≤ 10MB allowed) and the flow resumes;
- Records can be deleted or cleared.

## API

The **API** item in the more menu generates this app's API key and docs; trigger it externally with `POST /ext/v1/workflow/run` — see [API Reference · Workflow](../../api/workflow.md).

---

Previous: [Knowledge Base Q&A](../knowledge-base/kb-qa.md) ｜ Next: [Services & Tools](../mcp/mcp.md)
