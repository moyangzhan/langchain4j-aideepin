# Services & Tools (MCP)

> [← User Guide](../index.md) · [简体中文](../../../cn/guide/mcp/mcp.md)

MCP (Model Context Protocol) is a standard protocol for external tool integration. Once the admin registers MCP services, users **enable and configure** them on demand, letting the AI call those tools in chat (web search, database queries, third-party APIs…).

Open the **Tools** page from the left menu; two views at the top:

| View | Content |
|---|---|
| Services & tools | All services available in the system |
| My tools | Services you have configured |

> Users cannot create MCP services — only use the ones the admin provides; the list paginates.

## Viewing & Configuring

1. Click **Details** on a card to read the Markdown intro and related links;
2. Click **Configure** (**Enable** if never configured) to open the dialog:
   - **Intro** tab: service description;
   - **Configure** tab: the parameter table (parameter / value / sensitive — sensitive values are **stored encrypted**); services needing nothing say "No parameters required";
3. Fill in, set **Status** to **Enabled** (Stash = save without activating), confirm — "Configuration saved".

## Using in Chat

Tools must be checked on a **character** to be callable:

- Check them under **Services & Tools (MCP)** in [Character Settings](../chat/character-config.md#services--tools-mcp);
- Or click the **Tools** tag in the chat input bar.

Once configured, the AI calls tools as needed. Note DeepSeek's deep thinking is mutually exclusive with tool calls.

## API

The **API** item in the more menu generates a tools API key; `/ext/v1/mcp` returns the service list — see [API Reference · MCP Services](../../api/mcp.md).

---

Previous: [Apps & Workflows](../workflow/workflow.md) ｜ Next: [Model Selection](../model/model.md)
