# Q&A Import & Generation

> [← User Guide](../index.md) · [简体中文](../../../cn/guide/knowledge-base/qa-import.md)

Q&A-mode documents use question-answer pairs as the retrieval unit: hitting any question recalls its answer — ideal for FAQs and support scripts. Pairs come from three sources: **file import**, **AI generation**, **manual upkeep**.

## File Import

### Way 1: upload when creating a Q&A document

When [editing a document](document.md#editing-a-document) with segmentation mode **Q&A** and creating it new, you may attach an **import Q&A pairs** file (XLSX / CSV):

- **Download the template**: header row `question,answer`;
- **Multiple questions, one answer**: put each question on its own row and the answer on the first row (leave the rest blank);
- Each file becomes one standalone Q&A document;
- Uploading a file is mutually exclusive with "AI auto-generation": picking a file unchecks AI generation.

### Way 2: append to an existing document

On the [segment management](segment.md) page (Q&A pair list), **Import Q&A pairs** appends XLSX / CSV pairs to the current document; template download available.

## AI-Generated Pairs

The AI distills pairs from the document body (using the base's [ingest model](manage.md#3-document-index-settings-model)):

- Check **"Auto-generate Q&A pairs from the body after saving"** when creating;
- Once pairs exist the button becomes **"Clear current pairs and regenerate"** — with confirmation; the previous pairs are **unrecoverable**;
- Generation is asynchronous; check the pair list shortly after.

## Manual Maintenance

Click **Add Q&A pair** in the pair list:

1. **Questions**: one per line, **+Add question** for more — one answer can map to multiple questions; hitting any recalls the answer;
2. **Answer**: fill in and save.

Row actions on existing pairs: **Edit** (refills questions and answer), **Disable / Enable**, **Delete**.

## Tips

- Official material, accuracy first → curate and import manually;
- Lots of existing prose → AI-generate first, then proofread;
- New frequent questions → append manual pairs instead of regenerating everything.

---

Previous: [Import Documents](document.md) ｜ Next: [Segment Management](segment.md)
