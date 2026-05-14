# 贡献指南

感谢你对 LangChain4j-AIDeepin 的关注！欢迎以任何形式参与贡献。

## 如何贡献

- **Bug 报告** — 提交 Issue，描述问题现象和复现步骤
- **功能建议** — 提交 Issue，描述使用场景和期望行为
- **文档改进** — 直接提交 PR，修正错误或补充说明
- **代码贡献** — 按下方流程提交 PR

## 代码贡献

> 以下规范仅供参考。满足以下条件即可提交 PR：
> - 通过项目自带的 lint 检查（前端）或 Sonar / 类似工具检查（后端）
> - 本地编译及运行正常
> - 代码风格与现有代码保持一致

### 分支规范

- `feature/xxx` — 新功能
- `fix/xxx` — Bug 修复
- `docs/xxx` — 文档更新
- `refactor/xxx` — 代码重构

基于 `main` 分支创建。

### Commit 规范

遵循 [Conventional Commits](https://www.conventionalcommits.org/) 格式：

```
<type>(<scope>): <subject>
```

**type 类型：**

| 类型 | 说明 |
|:-----|:-----|
| feat | 新功能 |
| fix | Bug 修复 |
| docs | 文档更新 |
| style | 代码格式（不影响逻辑） |
| refactor | 重构（非新功能、非修复） |
| perf | 性能优化 |
| test | 测试相关 |
| build | 构建或依赖变更 |
| ci | CI 配置变更 |
| chore | 其他杂项 |
| revert | 回退提交 |
| wip | 进行中（开发未完成） |
| workflow | 工作流相关 |
| types | 类型定义变更 |
| release | 发布版本 |

**scope 范围（可选）：** `server`、`admin-web`、`user-web`、`docker`

**subject 要求：** 简明描述改动内容，不超过 108 个字符，支持中文。

**示例：**

```
feat(server): 添加 DeepSeek 模型适配
fix(admin-web): 修复知识库列表分页异常
docs: 更新部署文档
```

### 代码风格

总体与项目现有代码风格保持一致。

#### 后端（Java）

- 使用 IDE 默认格式化即可
- JDK 17 语法特性

#### 管理端前端（admin-web）

- Prettier + ESLint：2 空格缩进，无分号，单引号
- 检查代码：`pnpm lint`
- 自动修复：`pnpm lint:fix`

#### 用户端前端（user-web）

- @antfu ESLint：tab 缩进
- 检查代码：`pnpm lint`
- 自动修复：`pnpm lint:fix`

### PR 流程

1. Fork 本仓库
2. 基于 `main` 创建特性分支（如 `feature/add-xxx`）
3. 提交代码，commit message 遵循上述规范
4. 确保以下检查通过后再提交 PR：
   - **后端**：`mvn compile` 编译通过，本地启动无报错
   - **前端**：`pnpm lint` 无报错，`pnpm build` 构建通过
5. 推送到你的 Fork 仓库
6. 向本仓库 `main` 分支提交 Pull Request

**PR 描述建议包含：**

- 改动内容概述
- 关联的 Issue 编号（如 `Closes #123`）
- 测试说明（如何验证改动有效）

## Issue 规范

### Bug 报告

请包含以下信息：

- **环境**：操作系统、JDK 版本、浏览器、项目版本
- **复现步骤**：逐步描述如何触发问题
- **预期行为**：你期望发生什么
- **实际行为**：实际发生了什么（可附截图或日志）

### 功能建议

请包含以下信息：

- **背景**：遇到的问题或使用场景
- **期望行为**：你希望如何解决
- **替代方案**：你考虑过的其他方式（可选）
