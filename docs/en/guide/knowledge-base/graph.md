# Knowledge Graph

> [← User Guide](../index.md) · [简体中文](../../../cn/guide/knowledge-base/graph.md)

The graph index extracts **entities (vertices)** and **relations (edges)** from documents, complementing the vector index: vectors recall by semantic similarity, graphs organize by entity relations — good for multi-hop questions ("which policies cover the department of A's product lead").

## Generating a Graph

Graphs are extracted **per document**, consuming tokens of the base's [ingest model](manage.md#3-document-index-settings-model). Three entry points:

| Entry | Where |
|---|---|
| Check "Graph" | The [index dialog](document.md#indexing) when importing |
| **Graph** button | Document list actions (disabled until extracted) |
| "Click to generate graph" | The [segment management](segment.md) page's document card |

Using the segment page:

1. Click "Click to generate graph";
2. The confirmation names the model (translated): "The knowledge graph will be extracted using the base's ingest model '{model}'; this consumes that model's tokens. Start?";
3. Status shows **extracting** (background task) and refreshes on completion.

> 📷 Screenshot TODO: the graph extraction confirmation dialog. Replace with `![Graph confirm](../../../image/en/guide/knowledge-base/graph-01.png)` once added.

## Viewing the Graph

1. Click **View graph** from the document list or segment management;
2. The canvas shows vertices and relations;
3. Large graphs load in pages: "Vertices {loaded}/{total}, relations {loaded}/{total}" with a **Load more** button; **Re-layout** tidies the canvas;
4. Anomalous states show hints ("extracting, refresh later" or "no graph data yet").

> 📷 Screenshot TODO: the document graph page (canvas + load more). Replace with `![Knowledge graph](../../../image/en/guide/knowledge-base/graph-02.png)` once added.

## Graph Citations in Answers

After asking against a base with a graph index:

1. Click the **Graph** button under the answer;
2. The dialog shows the graph fragments referenced (entities with name/description, relations).

Characters linked to the base support this too. Together with **Citations** (hit segments) this fully traces the answer — see [Chat Window · Memory & Citations](../chat/window.md#memory--citations).

## Maintenance Notes

> [!WARNING]
> Graphs **do not auto-update** with body edits: after switching segmentation mode or editing the body, re-extract manually, or answers keep citing the old graph.

- Disabling a segment deletes its graph footprint; re-extract after re-enabling if needed (see [Segment Management](segment.md#enablingdisabling-segments));
- Graph cost scales with document length — prioritize entity-dense material (org structures, product specs, policy clauses); narrative text is fine with vectors only.

---

Previous: [Segment Management](segment.md) · Next: [Knowledge Base Q&A](kb-qa.md)
