# Segment Management

> [← User Guide](../index.md) · [简体中文](../../../cn/guide/knowledge-base/segment.md)

Open via **Segments** in the document list. The document card sits at the top; below it one of three lists depending on the segmentation mode.

**Document card**: summary, vector status, character count, vector hits and times, plus three entries:

| Entry | Description |
|---|---|
| View body | Read-only view, jumps to edit |
| View attachment | Preview the uploaded file |
| Generate / view graph | "Graph not generated yet — **click to generate**" before extraction; **View graph** after (see [Knowledge Graph](graph.md)) |

> 📷 Screenshot TODO: the segment management page (document card + segment table). Replace with `![Segment management](../../../image/en/guide/knowledge-base/segment-01.png)` once added.

## Plain Segments

Columns: #, segment text (folded to 3 lines, expandable), hit count, character count, status, actions.

| Action | Description |
|---|---|
| Edit | Modify the text; saving re-vectorizes automatically ("Saved, re-vectorizing") |
| Disable / Enable | See below |
| Delete | Remove the segment and its vectors |

## Q&A Pair List

Header buttons: **Add Q&A pair**, **Import Q&A pairs** (file append), **AI generate / regenerate**. Row actions and formats: [Q&A Import & Generation](qa-import.md).

## Parent-Child Segments

Parents are the top-level chunks, children are sub-chunks; children match precisely, the parent is returned as context.

| Action | On | Description |
|---|---|---|
| Edit parent | Parent | Modify parent content |
| Disable / Enable parent | Parent | Same as segment toggling, applied to the parent and its children |
| Delete parent | Parent | **Deletes the parent and all its children (including vectors)** |
| Re-split children | Parent | Deletes all children and their vectors and re-splits from the parent's **current content** |
| Add / edit / delete child | Child | Maintain a single child |

> [!WARNING]
> **Re-splitting children** overwrites manually added or edited children; make sure the parent content is final first.

## Enabling/Disabling Segments

Segments (and parents, pairs) can be toggled individually — a way to "take content offline temporarily". Confirmation texts differ by graph status (translated):

| Action | Document graphed | Document not graphed |
|---|---|---|
| Disable | Disabling will delete the segment's generated vectors and graph data. Disable? | Disabling will delete the segment's generated vector data. Disable? |
| Enable | Enabling will regenerate the segment's vectors and graph data (graph extraction consumes model quota). Enable? | Enabling will regenerate the segment's vector data. Enable? |

> [!TIP]
> Disable→enable is a costly cycle (vectors and graph are deleted then rebuilt). To take a whole document offline, use the document list's **Enabled** switch instead.

## Vector Repair & Failure Retry

- **Missing vector**: when a segment is flagged, the confirmation reads (translated): "This item's vector does not exist in the vector store (may not match the status shown). Rebuild the vector?" — confirm to repair;
- **Index failures**: the bottom failure panel lists recent failure reasons per dimension (vector / graph) with three retries: re-vectorize / re-graph / re-vectorize and re-graph; an empty pair list offers **AI generate** instead;
- While retrying, "Indexing: {dims}" shows and the page polls every 3 seconds.

> [!NOTE]
> Failures are mostly file-parsing issues or model timeouts. Occasional failures: just retry. Repeated failures: check the file (scanned PDFs) and model availability.

---

Previous: [Q&A Import & Generation](qa-import.md) · Next: [Knowledge Graph](graph.md)
