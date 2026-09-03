-- ============================================================
-- Migration: Add You.com as a web search engine provider and as an MCP
-- service market entry
-- ============================================================

-- 1. Web search engine provider setting (see all_dml.sql, mirrors google_setting)
-- https://documentation.you.com/api-reference/search
INSERT INTO adi_sys_config (name, value)
VALUES ('youcom_setting',
        '{"url":"https://ydc-index.io/v1/search","key":""}');

-- 2. MCP service market entry, remote MCP server for AI-native web search
-- (free profile, no API key required). You.com's endpoint speaks the
-- streamable-HTTP MCP transport, which this project's built-in 'sse'
-- transport (legacy GET-based SSE) cannot connect to (verified: a GET
-- request returns "405 Method Not Allowed: Use POST to send MCP requests").
-- It is connected here through the stdio bridge `npx -y mcp-remote <url>`,
-- the same install shape already used for Brave Search.
insert into adi_mcp (uuid, title, transport_type, stdio_command, stdio_arg, install_type, website,
                     remark, is_enable)
values (replace(gen_random_uuid()::text, '-', ''), 'You.com', 'stdio', 'npx',
        '-y mcp-remote https://api.you.com/mcp?profile=free', 'local',
        'https://you.com',
        '# You.com MCP Server

Remote MCP server providing AI-native web search via You.com.

## Features

- **you-search**: Web and news search with domain filtering (`include_domains`/`exclude_domains`),
  freshness controls (day/week/month/year or a date range), language/country selection, and
  optional live-crawl of matched pages (markdown or HTML).

## Free profile

This entry uses the `profile=free` endpoint (`https://api.you.com/mcp?profile=free`), which
requires no API key -- no configuration is needed before enabling it.

## Transport note

You.com''s MCP endpoint only speaks the streamable-HTTP transport (a plain GET returns
"405 Method Not Allowed"). Since this project''s built-in SSE transport expects a GET-based
event stream, it is connected here via the stdio bridge `npx -y mcp-remote <url>` instead,
mirroring how Brave Search is installed.',
        true);
