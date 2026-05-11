# 升级记录

## 版本变更

| 依赖 | 升级前 | 升级后 |
|------|--------|--------|
| Spring Boot | 3.4.2 | 3.5.14 |
| langchain4j (BOM) | 1.2.0 | 1.14.1 |
| langchain4j-community-* | 1.2.0-beta8 | 1.14.1-beta24 |
| langgraph4j | 1.5.3 | 1.8.15 |

## 代码变更

### pom.xml
- `spring-boot-starter-parent`: 3.4.2 → 3.5.14
- `langchain4j.version`: 1.2.0 → 1.14.1
- `tmp-langchain4j.version`: 1.2.0-beta8 → 1.14.1-beta24
- `langgraph4j.version`: 1.5.3 → 1.8.15

### adi-common/.../AbstractLLMService.java

**变更 1：ToolService.createContext 签名变更**

langchain4j 1.14.1 中 `ToolService.createContext` 方法签名从 `(String, UserMessage)` 变为 `(InvocationContext, UserMessage, List<ChatMessage>)`。

- 新增 import: `dev.langchain4j.invocation.InvocationContext`
- 使用 `InvocationContext.builder().chatMemoryId(id).timestampNow().build()` 构建上下文
- 原 `UuidUtil.createShort()` 作为 `chatMemoryId` 保留

**变更 2：McpClient.executeTool 返回值变更**

langchain4j-mcp 1.14.1-beta24 中 `McpClient.executeTool` 返回值从 `String` 变为 `ToolExecutionResult`。

- 新增 import: `dev.langchain4j.service.tool.ToolExecutionResult`
- 通过 `toolResult.resultText()` 获取文本结果

## 兼容性审查结果

### langchain4j (1.2.0 → 1.14.1)
- ✅ AiServices / @UserMessage / @SystemMessage — 兼容
- ✅ ChatMemory (MessageWindowChatMemory, TokenWindowChatMemory) — 兼容
- ✅ EmbeddingStore (PgVector, Neo4j) — 兼容
- ✅ RAG (ContentRetriever, EmbeddingStoreContentRetriever, DefaultRetrievalAugmentor) — 兼容
- ✅ StreamingChatResponseHandler — 接口稳定，新增 context 参数方法（均为 default）
- ✅ TokenStream — 兼容
- ✅ Document Parser (PDFBox, POI) — 兼容

### langgraph4j (1.5.3 → 1.8.15)
- ✅ StateGraph / addNode / addEdge / addConditionalEdges — 兼容
- ✅ CompileConfig / MemorySaver — 兼容
- ✅ NodeOutput / StreamingOutput — 兼容
- ✅ StreamingChatGenerator — 兼容

### Spring Boot (3.4.2 → 3.5.14)
- ✅ Spring MVC / Controller — 兼容
- ✅ Spring Data Redis — 兼容
- ✅ Spring Cache — 兼容
- ✅ Validation — 兼容
- ✅ Mail — 兼容
- ✅ MyBatis-Plus 3.5.12 — 兼容

## 待决事项

### 建议升级（非阻塞）
- [ ] **springdoc-openapi**: 当前 2.1.0（2023 年），建议升级到 2.6.x+ 以获得更好的 Spring Boot 3.5 兼容性
- [ ] **Spring Boot 3.5.x EOL**: 3.5.x 即将停止维护（约 2026 年 7-8 月），需规划后续升级到 4.0.x

### 已跳过（无影响）
- Spring Boot cache 配置格式（嵌套格式仍然有效）
- hutool-core 5.8.33（与本次升级无关）
