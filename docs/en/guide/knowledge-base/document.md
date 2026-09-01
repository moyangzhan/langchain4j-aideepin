# Import Documents

> [← User Guide](../index.md) · [简体中文](../../../cn/guide/knowledge-base/document.md)

From "My knowledge bases" click **Docs** to open the knowledge base detail (document list). The toolbar offers two ways to add:

- **Add (form)**: type title, summary and body — for small content;
- **Add (file)**: batch upload — for real imports.

## Uploading Files

Click **Add (file)** to open the dialog:

1. **Pick the segmentation mode**: plain / Q&A / parent-child (with usage hints, see [Overview](overview.md#three-segmentation-modes));
   - Q&A mode accepts XLSX / XLS / CSV only, with a **template download** (header `question,answer`); each file becomes one standalone Q&A document;
   - Parent-child mode sets the **child max tokens** (50–4000, default 200);
2. Drag or pick files: TXT, PDF, DOC, DOCX, XLS, XLSX, PPT, PPTX; each ≤ 10MB, at most 20 per batch;
3. Click **Upload and generate documents**.

Uploaded documents appear in the list with vector status **pending**.

## Indexing

Select documents and click **Index selected (N)**:

1. **Choose index type(s)**: vectorize / graph (both allowed);
   > Graph extraction uses an LLM to pull entities and relations and **consumes tokens**.
2. Confirm — "Index task running in background"; the list polls and refreshes.

Graphs can also be built later: the graph action is disabled until extracted, and extraction can be triggered from the [segment management](segment.md) page.

## Document List Columns

| Column | Description |
|---|---|
| Segmentation mode | Plain / Q&A / parent-child tag |
| Vector status | Pending / processing / vectorized / failed (with time) |
| Graph status | Whether the graph has been extracted |
| Attachment | The uploaded file; click to preview |
| Characters | Body length |
| Vector hits / graph hits | How often this document's segments were recalled |
| Enabled | Toggle. Off = excluded from retrieval (details in [Segment Management](segment.md#enablingdisabling-segments)) |

Actions: **Segments** (open segment management), **Graph** (view; disabled until extracted), **Edit**, **Delete**.

## Editing a Document

Available when adding by form or editing:

- **Title / summary / body**;
- **Segmentation mode**: switching warns — existing segments and vectors are discarded and rebuilt with the new mode; **the graph is not rebuilt automatically** and needs manual re-extraction;
- Q&A-mode extras: see [Q&A Import & Generation](qa-import.md);
- Parent-child **child max tokens**.

> In Q&A mode, editing the body **does not affect** generated Q&A pairs; update them via the pair list or AI regeneration.

---

Previous: [Create & Configure](manage.md) ｜ Next: [Q&A Import & Generation](qa-import.md)
