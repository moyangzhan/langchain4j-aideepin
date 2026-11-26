-- Add Keenable Search as an optional hosted MCP service.
-- Idempotent: existing installations with the canonical endpoint are unchanged.

insert into adi_mcp (uuid, title, transport_type, sse_url, sse_timeout, install_type, website, remark, is_enable)
select replace(gen_random_uuid()::text, '-', ''),
       'Keenable Search',
       'streamable_http',
       'https://api.keenable.ai/mcp-public',
       30,
       'remote',
       'https://keenable.ai',
       '# Keenable Search MCP

A remote MCP service for live web search and page fetching, served from Keenable''s own index. No account or API key is required.

## Tools

- **search_web_pages**: Search the web for current pages, optionally narrowed by site and publish date.
- **fetch_page_content**: Read a URL as clean Markdown.',
       true
where not exists (
    select 1
    from adi_mcp
    where sse_url = 'https://api.keenable.ai/mcp-public'
);
