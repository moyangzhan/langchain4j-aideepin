> **[🇨🇳 中文文档](README.zh-CN.md)** | English

## Introduction

This directory contains the admin dashboard for [LangChain4j-AIDeepin](../README.md), built on [naive-ui-admin](https://github.com/jekip/naive-ui-admin).

Default admin account: catkeeper@aideepin.com / Password: 123456

## Repository Structure

```
langchain4j-aideepin/
  ├── server/         Backend service
  ├── admin-web/      Admin dashboard ← This directory
  └── user-web/       User-facing web app
```

- Backend: [server/](../server/)
- User Web: [user-web/](../user-web/)

## Features

- **Dashboard** — System overview with key metrics including user activity, Token usage, and model usage statistics
- **User Management** — View user list, manage user status (enable/disable), and allocate quotas
- **Model Management**
  - Platform Configuration: Manage API Keys, Base URLs, and other parameters for AI platforms (OpenAI, DeepSeek, DashScope, SiliconFlow, Ollama, etc.)
  - Model List: Configure specific models under each platform, including enable/disable, set as free model, sorting, etc.
- **Knowledge Base Management** — View all knowledge bases in the system with document counts, vectorization status, and enable/disable management
- **Character Management** — View all user characters with message history; manage preset characters (system-predefined AI role templates)
- **Workflow Management** — Manage AI workflow components and process lists, support enabling/disabling workflows
- **MCP Management** — Manage MCP (Model Context Protocol) servers, configure SSE/stdio type MCP services
- **System Configuration**
  - Storage: Local storage or Alibaba Cloud OSS
  - ASR: Speech recognition service parameters
  - TTS: Text-to-speech service parameters (client-side/server-side synthesis)
  - Quota: Free quotas and paid strategies for user Token and image generation
  - Rate Limiting: Frequency limits for text requests and image generation

## Tech Stack

- Vue 3 + TypeScript
- [Naive UI](https://www.naiveui.com/)
- Vite
- Pinia
- Vue Router
- ECharts
- vue-i18n (Chinese/English supported)

## Prerequisites

### Node

`node` v20+

```shell
node -v
```

### PNPM

If you haven't installed `pnpm`:

```shell
npm install pnpm -g
```

## Install Dependencies

Run the following command in the root directory:

```shell
pnpm bootstrap
```

## Local Development

1. Modify `VITE_GLOB_API_URL` in the `.env` file to your actual backend address

2. Run the following command in the root directory:

```shell
pnpm dev
```

## Production

### Docker

See [docker/README.md](docker/README.md)

### Manual Build

1. Nginx Configuration

   Refer to the `nginx.conf` file in the root directory. Change `localhost:9999` in `proxy_pass http://localhost:9999/;` to your backend service's IP and port.

   **If admin web and user web share the same nginx**, use this configuration:

```shell
# adi-web: user web build output
# adi-admin-web: admin web build output

# User Web
# URL: http://your-ip:port/
location / {
  root /usr/share/nginx/adi-web;
  try_files $uri /index.html;
}

# Admin Web
# URL: http://your-ip:port/admin
  location /admin/ {
    alias /usr/share/nginx/adi-admin-web/;
   	index /index.html;
  }

# Backend API
location /api/ {
  proxy_set_header X-Real-IP $remote_addr;
  proxy_pass http://localhost:9999/;
}
```

2. Run the following command in the root directory ([reference](https://vitejs.dev/guide/static-deploy.html#building-the-app)):

```shell
pnpm build
```

3. Copy the contents of the `dist` folder to the root directory of your web server.

   The web server root directory is configured in `nginx.conf` under `location /`.

## Browser Support

`Chrome 80+` is recommended for local development.

Supports modern browsers, does not support IE.

| ![IE](https://raw.githubusercontent.com/alrra/browser-logos/master/src/edge/edge_48x48.png) IE | ![Edge](https://raw.githubusercontent.com/alrra/browser-logos/master/src/edge/edge_48x48.png) Edge | ![Firefox](https://raw.githubusercontent.com/alrra/browser-logos/master/src/firefox/firefox_48x48.png) Firefox | ![Chrome](https://raw.githubusercontent.com/alrra/browser-logos/master/src/chrome/chrome_48x48.png) Chrome | ![Safari](https://raw.githubusercontent.com/alrra/browser-logos/master/src/safari/safari_48x48.png) Safari |
| :------------------------------------------------------------------------------------------: | :----------------------------------------------------------------------------------------------: | :---------------------------------------------------------------------------------------------------------: | :-----------------------------------------------------------------------------------------------------: | :-----------------------------------------------------------------------------------------------------: |
|                                         not support                                         |                                         last 2 versions                                         |                                               last 2 versions                                               |                                             last 2 versions                                             |                                             last 2 versions                                             |

