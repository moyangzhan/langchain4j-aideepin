> 中文文档 | **[🇬🇧 English](README.md)**

## 简介

本目录是 [LangChain4j-AIDeepin](../README.zh-CN.md) 的用户端 WEB 前端项目。

## 仓库结构

```
langchain4j-aideepin/
  ├── server/         后端服务
  ├── admin-web/      管理端 WEB
  └── user-web/       用户端 WEB ← 本目录
```

- 后端服务：[server/](../server/)
- 管理端 WEB：[admin-web/](../admin-web/)

## 前置要求

### Node

`node` v20+

推荐使用使用 [nvm](https://github.com/nvm-sh/nvm) 管理本地多个 `node` 版本

### PNPM

pnpm v9+

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

1、修改根目录下 `.env` 文件中的 `VITE_GLOB_API_URL` 为你的实际后端口地址

2、根目录下运行以下命令

```shell
pnpm dev
```

3、如后端服务为远程地址，使用nginx解决跨域问题

nginx配置参考 [./docker/nginx/nginx.conf](docker/nginx/nginx.conf)

## 正式环境

### Docker 运行

详见 [docker/README.zh-CN.md](docker/README.zh-CN.md)

### 手动打包

1、 nginx配置

服务器上nginx的配置可以参考 `./docker/nginx/nginx.conf`，将 `proxy_pass http://localhost:9999/;` 中的 `localhost:9999`改成后端服务对应的ip及端口

**如果管理端WEB跟用户端WEB使用同一个nginx**，可参考以下配置：

```shell
# adi-web存放的是用户端构建后的代码
# adi-admin-web存放的是管理端构建后的代码

# 用户端WEB页面配置
# 访问地址：http://你的ip:port/
location / {
  root /usr/share/nginx/adi-web;
  try_files $uri /index.html;
}

# 管理端WEB页面配置
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

## 常见问题

Q: 如果只使用前端页面，在哪里改请求接口？

A: 根目录下 `.env` 文件中的 `VITE_GLOB_API_URL` 字段。

Q: 文件保存时全部爆红?

A: `vscode` 请安装项目推荐插件，或手动安装 `Eslint` 插件。

Q: 前端没有打字机效果？

A: 一种可能原因是经过 Nginx 反向代理，开启了 buffer，则 Nginx 会尝试从后端缓冲一定大小的数据再发送给浏览器。请尝试在反代参数后添加 `proxy_buffering off;`，然后重载 Nginx。其他 web server 配置同理。

