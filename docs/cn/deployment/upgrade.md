# 升级

> [← 部署](index.md) · [English](../../en/deployment/upgrade.md)

升级面向有开发经验的部署人员，是一个由你统筹的原子过程：需要在同一个维护窗口内完成「数据库迁移 SQL → 服务端 → 前端」三者的替换，任何新旧混搭的中间态都可能出错——例如新后端的接口变更会让旧前端解析失败，重命名数据表的迁移脚本一旦执行，旧后端的查询会立刻报错。

## 1. 确定当前版本与目标版本

- 当前已部署的版本决定迁移起点：所有编号不大于当前版本的增量脚本（`NNN_*.sql`）都已执行过
- 需要执行的迁移脚本 = 两个版本之间新增的增量脚本：

  ```bash
  git diff --name-only <当前版本>..<目标版本> -- server/db_migration
  ```
- 迁移脚本位于 [server/db_migration](https://github.com/moyangzhan/langchain4j-aideepin/tree/main/server/db_migration)，命名规范与执行方式见该目录 [README](https://github.com/moyangzhan/langchain4j-aideepin/blob/main/server/db_migration/README.zh-CN.md)

## 2. 升级前准备

1. 备份数据库——用于灾难恢复（升级失败或数据损坏时重建）：

   ```bash
   pg_dump -h <host> -U <user> -d aideepin -F c -f aideepin_backup_$(date +%Y%m%d).dump
   ```

2. Docker 部署建议同时更新 `.env` 中的 `IMAGE_VERSION`：镜像按该值打标签，逐版本递增可保留旧版本镜像

## 3. 执行升级

在维护窗口内按序完成，期间服务不可用：

1. **停止服务**。一键部署在根目录 `docker/` 执行 `docker compose down`；Docker 独立部署在各子项目 `docker/` 内执行；裸机部署停止后端进程
2. **执行数据库迁移**：从当前版本对应的编号之后，按编号顺序执行所有增量迁移脚本（见 db_migration README）
3. **更新代码到目标版本**：`git checkout <tag 或 commit>`
4. **重新构建并启动**：
   - 一键部署：根目录 `docker/` 内 `docker compose up -d --build`
   - Docker 独立部署：各子项目 `docker/` 内 `docker compose up -d --build`
   - 裸机部署：`mvn clean package -Dmaven.test.skip=true` 后替换 JAR 并启动（见[裸机部署](bare-metal.md)）
5. **验证**：登录用户端与管理端，检查核心功能（对话、知识库、工作流）正常

## 4. 关于回退

**本项目不支持数据库回退**：迁移脚本只包含前进方向，没有回滚脚本，已执行的迁移无法撤销；数据库结构也不会随服务端回退。「旧版本服务端/前端 + 已迁移的数据库」的任何组合都不受支持——即使迁移看起来只是新增，也可能修改旧版本依赖的数据。因此：

- 升级前先在测试环境完整演练，确认目标版本可用后再操作生产环境
- 升级前的数据库备份用于灾难恢复（重建数据库、找回数据），不是版本回退手段；用它回到升级前状态会丢失升级后产生的全部数据，属自担风险的手工操作

---

上一篇：[裸机部署](bare-metal.md)
