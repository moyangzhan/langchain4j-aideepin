# i18n 改造 — 需要用户决策的问题

## 后端

### Q1：AdiConstant SSEEventData 硬编码中文
**位置**: `AdiConstant.SSEEventData` 中 `STATE_QUESTION_ANALYSING`（"问题分析中"）、`STATE_KNOWLEDGE_SEARCHING`（"知识库搜索中"）、`STATE_THINKING`（"推理中"）、`STATE_RESPONDING`（"回答中"）
**问题**: 这些值通过 SSE 发送到前端，前端根据值做判断。如果改成英文或 i18n key，前端需要同步改判断逻辑。
**选项**:
- A：改为英文（前端同步修改判断逻辑）
- B：改为 i18n key（前端根据 key 查找显示文本）
- C：保持不变（前端做映射）

### Q2：AdiConstant.DEFAULT_NAME "通用智能助手"
**位置**: `AdiConstant.ConversationConstant.DEFAULT_NAME`
**问题**: 创建默认对话时使用的名称。是否需要根据用户 locale 切换？
**选项**:
- A：需要 locale 感知（中文用户看到"通用智能助手"，英文用户看到 "General AI Assistant"）
- B：保持中文不变（对话名称不影响功能）

### Q3：ConversationMessageService.buildMemoryAndKnowledge 中的 "无"
**位置**: `ConversationMessageService.java` 第 651、660 行
**问题**: 当知识库/记忆为空时使用 "无" 填入 Prompt。需要将 locale 传入该方法来切换 "无"/"None"。
**选项**:
- A：传入 locale，根据语言切换（改动涉及方法签名变化）
- B：统一改为 "None"（英文在中文 Prompt 中也能被 LLM 理解）
- C：保持不变

### Q4：admin 后端 9 个 Controller 缺少 @Tag/@Operation 注解
**问题**: AdminConvController、AdminKbController、AdminModelController 等 9 个 Controller 完全没有 @Tag 和 @Operation 注解。
**选项**:
- A：现在补充双语 @Tag/@Operation（工作量较大，约 40+ 个方法）
- B：暂不补充（admin API 文档优先级较低）

## Web 前端

### Q5：web 前端仍有约 120+ 处硬编码中文
**问题**: 审查发现 web 前端在 ai-search、knowledge-base、mcp、draw、gallery、workflow、user、chat 等模块仍有大量硬编码中文未替换为 $t() 调用。
**选项**:
- A：全部修复（预计需要修改 30+ 个 .vue 文件，新增 50+ 个翻译 key）
- B：分批修复（先修核心页面如 chat、knowledge-base，再修次要页面）

## Admin 前端

### Q6：admin 前端 system/ 目录（10 个 .vue）和 columns.ts（7 个文件）完全未改造
**问题**: 这些文件包含大量硬编码中文，覆盖系统设置（ASR/TTS/存储/额度/限流）和所有表格列定义。
**选项**:
- A：全部修复
- B：暂不处理（功能优先级较低）
