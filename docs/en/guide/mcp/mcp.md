# Services & Tools (MCP)

> [← User Guide](../index.md) · [简体中文](../../../cn/guide/mcp/mcp.md)

MCP (Model Context Protocol) is a standard protocol for external tool integration. Once the admin registers MCP services, users **enable and configure** them on demand, letting the AI call those tools in chat (web search, map queries, database lookups…).

Open the **Tools** page from the left menu; two views at the top:

| View | Content |
|---|---|
| Services & tools | All services available in the system |
| My tools | Services you have configured |

> [!NOTE]
> Users cannot create MCP services — only use the ones the admin provides; the list paginates ("maximum page limit exceeded" means narrow down or retry later).

## Viewing & Configuring

1. Click **Details** on a card to read the Markdown intro and related links;
2. Click **Configure** (**Enable** if never configured) to open the dialog:
   - **Intro** tab: the service description (Markdown);
   - **Configure** tab: the **service parameters** table (refer to the Intro tab) — parameter / value / sensitive; services needing nothing say "This service can be used without any parameters.";
3. Fill in the values (input hint: "Enter the value for {parameter}");
4. Set **Status**:
   - **Enabled**: saved and active;
   - **Stash**: draft only, not active;
5. Confirm — "Configuration saved".

> [!NOTE]
> Parameters marked **sensitive** (tooltip: "sensitive information is stored encrypted") such as API keys are encrypted at rest and never echoed in plaintext.

> 📷 Screenshot TODO: the MCP configure dialog (Intro / Configure tabs). Replace with `![MCP config](../../../image/en/guide/mcp/mcp-01.png)` once added.

## Using in Chat

Tools must be checked on a **character** to be callable (tooltip, translated: "Checked items mean this character may use the tools in those services"):

- Check them under **Services & Tools (MCP)** in [Character Settings](../chat/character-config.md#services--tools-mcp);
- Or click the **Tools** tag in the chat input bar ("Configure the services & tools for the character").

Once configured, the AI calls tools as needed.

> [!WARNING]
> DeepSeek's deep thinking is mutually exclusive with tools/web search: with thinking on, "DeepSeek deep thinking does not support tool calls; disable deep thinking first" — or tool calling is auto-disabled.

## API

The **API** item in the more menu generates a tools API key; `/ext/v1/mcp` returns the service list (all supported / active for the current user) — see [API Reference · MCP Services](../../api/mcp.md).

---

Previous: [Apps & Workflows](../workflow/workflow.md) ｜ Next: [Model Selection](../model/model.md)
