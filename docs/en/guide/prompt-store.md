# Prompt Store

> [← User Guide](index.md) · [简体中文](../../cn/guide/prompt-store.md)

Click the **Prompt Store** icon at the bottom of the left menu to open the dialog for managing reusable prompts. Two tabs:

| Tab | Content |
|---|---|
| Local | Prompts you saved |
| Online | The system's online prompt library |

## Managing Prompts

- **Add**: enter a title and the prompt text; duplicates are rejected ("Title duplicated, please re-enter" / "Content duplicated: {msg}, please re-enter");
- **Edit / Delete**: maintain entries;
- **Search**: filter by keyword.

## Import & Export

| Action | Description |
|---|---|
| Local import | Paste prompt JSON in the dialog to bulk import; malformed input reports "JSON format error, please check the JSON format" |
| Online import | Pull entries from the online library into local |
| Export | Download your local prompts as `export_prompts.json` for backup or migration |

Example of the import JSON format:

```json
[
  {
    "act": "Act as an English translator and improver",
    "prompt": "I want you to act as an English translator, spelling corrector and improver."
  }
]
```

> [!TIP]
> The JSON format is compatible with popular prompt collections (such as the localized versions of awesome-chatgpt-prompts) — community prompt packs can be reused directly.

---

Previous: [Quota & Usage](account/usage.md) · Next: [Admin Console](admin/admin.md)
