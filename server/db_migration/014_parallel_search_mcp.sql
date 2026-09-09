-- Add Parallel Search as an optional hosted MCP service.
-- Idempotent: existing installations with the canonical endpoint are unchanged.

insert into adi_mcp (uuid, title, transport_type, sse_url, sse_timeout, install_type, website, remark, is_enable)
select replace(gen_random_uuid()::text, '-', ''),
       'Parallel Search',
       'streamable_http',
       'https://search.parallel.ai/mcp',
       30,
       'remote',
       'https://parallel.ai',
       '# Parallel Search MCP

A free remote MCP service for live web search and URL fetching. No account or API key is required.

## Tools

- **web_search**: Search the web for current information.
- **web_fetch**: Extract clean Markdown from a URL.',
       true
where not exists (
    select 1
    from adi_mcp
    where sse_url = 'https://search.parallel.ai/mcp'
);
