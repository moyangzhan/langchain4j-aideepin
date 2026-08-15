-- ============================================================
-- Migration: 新增 You.com 网络搜索引擎服务商，以及 MCP 服务市场条目
-- ============================================================

-- 1. 网络搜索引擎配置（见 all_dml.sql，与 google_setting 保持一致的结构）
-- https://documentation.you.com/api-reference/search
INSERT INTO adi_sys_config (name, value)
VALUES ('youcom_setting',
        '{"url":"https://ydc-index.io/v1/search","key":""}');

-- 2. MCP 服务市场条目，远程MCP服务器，提供AI原生的网络搜索能力（免费档，无需API密钥）
-- You.com的端点使用的是streamable-HTTP传输协议，本项目内置的'sse'传输方式（基于GET的旧版SSE）
-- 无法连接（已验证：直接GET请求会返回"405 Method Not Allowed: Use POST to send MCP requests"）。
-- 因此这里通过stdio桥接方式`npx -y mcp-remote <url>`接入，与Brave Search的安装方式一致。
insert into adi_mcp (uuid, title, transport_type, stdio_command, stdio_arg, install_type, website,
                     remark, is_enable)
values (replace(gen_random_uuid()::text, '-', ''), 'You.com', 'stdio', 'npx',
        '-y mcp-remote https://api.you.com/mcp?profile=free', 'local',
        'https://you.com',
        '# You.com MCP 服务器

远程 MCP 服务器，通过 You.com 提供 AI 原生的网络搜索能力。

## 功能

- **you-search**：网页与新闻搜索，支持按域名过滤（include_domains/exclude_domains）、
  新鲜度过滤（day/week/month/year 或指定日期范围）、语言/国家选择，以及对命中页面的
  可选实时抓取（markdown 或 HTML 格式）。

## 免费档

本条目使用 `profile=free` 端点（`https://api.you.com/mcp?profile=free`），无需 API 密钥，
启用前无需任何配置。

## 传输方式说明

You.com 的 MCP 端点仅支持 streamable-HTTP 传输（直接 GET 请求会返回 "405 Method Not
Allowed"）。由于本项目内置的 sse 传输方式依赖基于 GET 的事件流，因此这里改用 stdio 桥接
`npx -y mcp-remote <url>` 的方式接入，与 Brave Search 的安装方式一致。',
        true);
