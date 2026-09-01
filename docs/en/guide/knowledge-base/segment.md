# Segment Management

> [← User Guide](../index.md) · [简体中文](../../../cn/guide/knowledge-base/segment.md)

Open via **Segments** in the document list. The document card at the top shows: summary, vector status, character count, vector hits and times; plus **View body** (read-only, jumps to edit), **View attachment** (file preview); if no graph yet — "Graph not generated, **click to generate**", otherwise **View graph** (see [Knowledge Graph](graph.md)).

The page shows one of three lists depending on the document's segmentation mode.

## Plain Segments

Columns: #, segment text (folded to 3 lines, expandable), hit count, character count, status, actions.

| Action | Description |
|---|---|
| Edit | Modify the text; saving re-vectorizes automatically |
| Disable / Enable | See below |
| Delete | Remove the segment and its vectors |

## Q&A Pair List

See [Q&A Import & Generation](qa-import.md): add (multi-question per answer), file append, AI generate / regenerate, edit, disable / enable, delete.

## Parent-Child Segments

Parents are the top-level chunks, children are sub-chunks; children match precisely, the parent is returned as context.

| Action | Description |
|---|---|
| Edit parent | Modify parent content |
| Disable / Enable parent | Same as segment toggling, applied to the whole parent |
| Delete parent | **Deletes the parent and all its children (including vectors)** |
| Re-split children | Deletes all children and their vectors and re-splits from the parent's current content; **manually added or edited children are overwritten** |
| Add / edit / delete child | Maintain a single child |

## Enabling/Disabling Segments

Segments (and parents, pairs) can be toggled individually:

- **Disable**: the segment stops being retrieved; its vectors and graph footprint are deleted ("This will delete the segment's generated vector (and graph) data");
- **Enable**: vectors and graph are regenerated ("graph extraction consumes model quota").

## Vector Repair & Failure Retry

- **Missing vector**: when a segment is flagged, click **confirm rebuild** to repair;
- **Index failures**: the failure panel at the bottom lists recent failure reasons per dimension (vector / graph) with **Re-vectorize / Re-graph / Re-vectorize and re-graph** retries; an empty pair list offers **AI generate** instead;
- While retrying, "Indexing: {dims}" shows and the page polls every 3 seconds.

---

Previous: [Q&A Import & Generation](qa-import.md) ｜ Next: [Knowledge Graph](graph.md)
