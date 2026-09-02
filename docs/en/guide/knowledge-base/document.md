# Import Documents

> [← User Guide](../index.md) · [简体中文](../../../cn/guide/knowledge-base/document.md)

From "My knowledge bases" click the title or **Docs** to open the knowledge base detail (document list). The toolbar offers two ways to add:

- **Add (form)**: type title, summary and body — for single short pieces;
- **Add (file)**: batch upload — for real imports.

> 📷 Screenshot TODO: the knowledge base detail page (document table + toolbar). Replace with `![Document list](../../../image/en/guide/knowledge-base/document-01.png)` once added.

## Uploading Files

1. Click **Add (file)** to open the dialog (card title "Upload documents to generate documents").
2. **Pick the segmentation mode**. The grey hint under the dropdown changes with the mode; in-app texts (translated):

| Mode | In-app hint (translated) |
|---|---|
| Plain | Plain: for continuous content (articles, manuals); chunks per the split parameters, each chunk vectorized and recalled directly |
| Q&A | Q&A: for FAQ-style content organized as pairs; questions are vectorized and recalled, answers returned as-is |
| Parent-child | Parent-child: for long documents; children match precisely and the hit returns the parent as full context |

   - **Q&A** accepts XLSX / XLS / CSV only (with **template download**); each file becomes one standalone Q&A document named after the file — format details in [Q&A Import & Generation](qa-import.md);
   - **Parent-child** shows **child max tokens**, hint (translated): "The parent is split into children of this size; children match precisely and the hit returns the parent as full context; range 50–4000, default 200; changes take effect on re-vectorization";
   - **Plain** follows the base's split settings, no extra parameters.
3. Drag files into the upload area (or click to browse).

| Limit | Value |
|---|---|
| Formats | TXT, PDF, DOC, DOCX, XLS, XLSX, PPT, PPTX |
| Size | ≤ 10MB each |
| Count | At most 20 per batch |

4. Click **Upload and generate documents**. Documents appear in the list with vector status **pending**.

> 📷 Screenshot TODO: the upload dialog (mode selection + drop area). Replace with `![Upload dialog](../../../image/en/guide/knowledge-base/document-02.png)` once added.

> [!TIP]
> Scanned (image-only) PDFs cannot be text-extracted — import ends with 0 characters and nothing retrieved. OCR them first.

## Adding by Form

Click **Add (form)** for the document edit page: title, summary, body and segmentation mode — see [Editing a Document](#editing-a-document). In Q&A mode you can also upload a QA file or enable AI generation, see [Q&A Import & Generation](qa-import.md).

## Indexing

Uploading only creates documents and segments — **they become retrievable once indexed**:

1. Check one or more documents in the list;
2. Click **Index selected (N)**;
3. **Choose index type(s)**: vectorize / graph (both allowed). The dialog's in-app note (translated):

> [!NOTE]
> "Graphing documents uses a large language model and consumes a certain amount of tokens" — vectorize first to validate retrieval, then graph as needed.

4. Confirm — "Index task running in background"; the list polls every 3 seconds.

> 📷 Screenshot TODO: the index dialog (type checkboxes + selected documents). Replace with `![Index dialog](../../../image/en/guide/knowledge-base/document-03.png)` once added.

Un-graphed documents can be indexed later from the [segment management](segment.md) page's document card.

## Document List Columns

| Column | Description |
|---|---|
| Segmentation mode | Plain / Q&A / parent-child tag |
| Vector status | Pending / processing / vectorized / failed (with time) |
| Graph status | Whether the graph has been extracted |
| Attachment | The uploaded file; click to preview |
| Characters | Body length — the first indicator of healthy parsing |
| Vector hits / graph hits | How often this document's segments were recalled — long-standing 0 means content doesn't match questions |
| Enabled | Toggle. Off = excluded from retrieval (consequences in [Segment Management](segment.md#enablingdisabling-segments)) |
| Created / updated | Recent changes |

Actions: **Segments** (segment management), **Graph** (view; disabled with "Not graphed yet" until extracted), **Edit**, **Delete**.

## Editing a Document

Via **Edit** in the actions column (or when adding by form):

| Field | Description |
|---|---|
| Title / summary / body | The body feeds segmentation and indexing |
| Segmentation mode | Switching warns: "Switching will discard the document's existing segments and vector indexes and rebuild with the new mode; **the graph is not rebuilt automatically and must be re-extracted manually**" |
| Child max tokens | Parent-child mode only |

- Q&A-mode extras (AI generation, QA file import): see [Q&A Import & Generation](qa-import.md);
- Saving body edits re-vectorizes affected segments automatically.

> [!WARNING]
> In Q&A mode, editing the body **does not affect** generated pairs. Update pairs via the pair list or AI regeneration (which clears existing pairs).

---

Previous: [Create & Configure](manage.md) · Next: [Q&A Import & Generation](qa-import.md)
