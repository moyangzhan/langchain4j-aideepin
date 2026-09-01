# Knowledge Graph

> [← User Guide](../index.md) · [简体中文](../../../cn/guide/knowledge-base/graph.md)

The graph index extracts **entities (vertices)** and **relations (edges)** from documents, complementing the vector index: vectors recall by semantic similarity, the graph organizes by entity relations — good for multi-hop questions.

## Generating a Graph

Graphs are extracted **per document**, consuming tokens of the base's [ingest model](manage.md#3-document-index-settings-model). Three entry points:

1. Check "Graph" in the [index dialog](document.md#indexing) when importing;
2. The **Graph** action in the document list (disabled until extracted);
3. "Click to generate graph" on the document card of the [segment management](segment.md) page.

The confirmation names the ingest model ("Extract a knowledge graph using model {model}… Continue?"). Extraction runs in the background; the status shows **extracting** and refreshes on completion.

## Viewing the Graph

Click **View graph** from the document list or segment management:

- A canvas of vertices and relations;
- Large graphs load in pages: "Vertices {loaded}/{total}, relations {loaded}/{total}" with a **Load more** button;
- Anomalous states show hints ("extracting, refresh later" or "no graph data yet").

## Graph Citations in Answers

After asking against a base with a graph index, the **Graph** button under the answer shows the graph fragments referenced; characters linked to the base support this too. Together with **Citations** (hit segments) this fully traces the answer — see [Chat Window · Memory & Citations](../chat/window.md#memory--citations).

## Maintenance Notes

- Graphs **do not auto-update** with body edits: after switching segmentation mode or editing the body, re-extract manually;
- Disabling a segment deletes its graph footprint; re-extract if needed after enabling (see [Segment Management](segment.md#enablingdisabling-segments)).

---

Previous: [Segment Management](segment.md) ｜ Next: [Knowledge Base Q&A](kb-qa.md)
