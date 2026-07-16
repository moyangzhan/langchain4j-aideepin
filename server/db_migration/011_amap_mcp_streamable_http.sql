-- ============================================================
-- amap (Gaode Maps) MCP: switch transport from SSE to Streamable HTTP.
-- The /sse endpoint only accepts GET; the streamable_http transport POSTs to /mcp,
-- so the preset amap MCP record must point at /mcp to initialize successfully.
-- Also refreshes the transport_type column comment and the amap remark text.
-- ============================================================
-- Idempotent: matches the old sse_url, so re-running on an already-upgraded DB is a no-op.
-- The remark REPLACE is a no-op on DBs whose remark lacks the old English phrase (e.g. CN).

COMMENT ON COLUMN adi_mcp.transport_type IS 'Transport type: sse, streamable_http, stdio';

UPDATE adi_mcp
SET transport_type = 'streamable_http',
    sse_url         = 'https://mcp.amap.com/mcp',
    remark          = REPLACE(remark, 'MCP (SSE) mode', 'MCP (Streamable HTTP) mode')
WHERE sse_url = 'https://mcp.amap.com/sse';
