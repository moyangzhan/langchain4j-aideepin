> 🇨🇳 中文 | **[English](README.md)**

## 简介

本目录是 [LangChain4j-AIDeepin](../README.zh-CN.md) 的管理后台 WEB 端，基于 [naive-ui-admin](https://github.com/jekip/naive-ui-admin)。

默认管理员账号：catkeeper@aideepin.com / 密码：123456

## 仓库结构

```
langchain4j-aideepin/
  ├── server/         后端服务
  ├── admin-web/      管理端 WEB ← 本目录
  └── user-web/       用户端 WEB
```

- 后端服务：[server/](../server/)
- 用户端 WEB：[user-web/](../user-web/)

## 功能

* **主控台** — 系统概览，包括用户活跃度、Token 消耗、模型使用统计等关键指标
* **用户管理** — 用户列表查看、状态管理（启用/禁用）、额度分配
* **模型管理**
  * 平台配置：管理各 AI 平台（OpenAI、DeepSeek、灵积、硅基流动、Ollama 等）的 API Key、Base URL 等参数
  * 模型列表：配置各平台下的具体模型，包括启用/禁用、设为免费模型、排序等
* **知识库管理** — 查看系统内所有知识库及其文档数、向量化状态，管理知识库的启用/禁用
* **角色管理** — 查看系统内所有用户角色，支持查看角色详情和消息记录；管理预设角色（系统预置的 AI 角色对话模板）
* **流程编排** — 管理 AI 工作流组件和流程列表，支持启用/禁用工作流
* **MCP 管理** — 管理 MCP（Model Context Protocol）服务端，配置 SSE/stdio 类型的 MCP 服务
* **系统配置**
  * 存储配置：本地存储或阿里云 OSS
  * ASR 配置：语音识别服务参数
  * TTS 配置：语音合成服务参数（客户端合成/服务端合成）
  * 额度配置：用户 Token、绘图次数的免费额度及付费策略
  * 限流策略：文本请求和图片生成的频率限制

## 技术栈

* Vue 3 + TypeScript
* [Naive UI](https://www.naiveui.com/)
* Vite
* Pinia
* Vue Router
* ECharts
* vue-i18n（支持中文/英文）

## 前置要求

### Node

`node` v20+

```shell
node -v
```

### PNPM

如果你没有安装过 `pnpm`

```shell
npm install pnpm -g
```

## 安装依赖

根目录下运行以下命令

```shell
pnpm bootstrap
```

## 本地环境开发

1、修改根目录下 `.env` 文件中的 `VITE_GLOB_API_URL` 为你的实际后端服务地址

2、根目录下运行以下命令

```shell
pnpm dev
```

## 正式环境

### Docker 运行

详见 [docker/README.zh-CN.md](docker/README.zh-CN.md)

### 手动打包

1、 nginx 配置

服务器上 nginx 的配置可以参考根目录下的 `nginx.conf` 文件，将 `proxy_pass http://localhost:9999/;` 中的 `localhost:9999` 改成后端服务对应的 IP 及端口

**如果管理端 WEB 跟用户端 WEB 使用同一个 nginx**，可参考以下配置：

```shell
# adi-web 存放的是用户端构建后的代码
# adi-admin-web 存放的是管理端构建后的代码

# 用户端 WEB 页面配置
# 访问地址：http://你的ip:port/
location / {
  root /usr/share/nginx/adi-web;
  try_files $uri /index.html;
}

# 管理端 WEB 页面配置
# 访问地址：http://你的ip:port/admin
  location /admin/ {
    alias /usr/share/nginx/adi-admin-web/;
   	index /index.html;
  }

# 后端服务
location /api/ {
  proxy_set_header X-Real-IP $remote_addr; #转发用户IP
  proxy_pass http://localhost:9999/;
}
```

2、根目录下运行以下命令，[参考信息](https://cn.vitejs.dev/guide/static-deploy.html#building-the-app)

```shell
pnpm build
```

3、将 `dist` 文件夹内的文件复制到网站服务的根目录下

网站服务的根目录：`nginx.conf` 的 `location /` 设置的目录

## 浏览器支持

本地开发推荐使用 `Chrome 80+` 浏览器

支持现代浏览器，不支持 IE

| ![IE](https://raw.githubusercontent.com/alrra/browser-logos/master/src/edge/edge_48x48.png) IE | ![Edge](https://raw.githubusercontent.com/alrra/browser-logos/master/src/edge/edge_48x48.png) Edge | ![Firefox](https://raw.githubusercontent.com/alrra/browser-logos/master/src/firefox/firefox_48x48.png)Firefox | ![Chrome](https://raw.githubusercontent.com/alrra/browser-logos/master/src/chrome/chrome_48x48.png)Chrome | ![Safari](https://raw.githubusercontent.com/alrra/browser-logos/master/src/safari/safari_48x48.png)Safari |
| :------------------------------------------------------------------------------------------: | :----------------------------------------------------------------------------------------------: | :---------------------------------------------------------------------------------------------------------: | :-----------------------------------------------------------------------------------------------------: | :-----------------------------------------------------------------------------------------------------: |
|                                         not support                                         |                                         last 2 versions                                         |                                               last 2 versions                                               |                                             last 2 versions                                             |                                             last 2 versions                                             |

