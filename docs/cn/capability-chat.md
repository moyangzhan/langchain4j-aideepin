# 对话 / 图像识别能力接入

对话（`text`）和图像识别（`vision`）使用同一套 LLM 基础设施，共享同一套接入逻辑。

## 是否需要写代码？

| 平台兼容 OpenAI API | 平台不兼容 OpenAI API |
|:------------------:|:-------------------:|
| 不需要 | **需要** |

- **兼容 OpenAI API 的平台**：只需在 `adi_model_platform` 中将 `is_openai_api_compatible` 设为 `true`，系统启动时通过 `OpenAiCompatibleLLMService` 自动加载，**零代码**。
- **不兼容的平台**：需要编写 Service 类，见下文。

## 前置步骤：注册平台名称常量

在 `AdiConstant.ModelPlatform` 中添加新平台名称常量：

**文件**: `adi-common/src/main/java/com/moyz/adi/common/cosntant/AdiConstant.java`

```java
public static class ModelPlatform {
    public static final String DEEPSEEK = "deepseek";
    public static final String OPENAI = "openai";
    public static final String DASHSCOPE = "dashscope";
    public static final String OLLAMA = "ollama";
    public static final String SILICONFLOW = "siliconflow";
    // 新增
    public static final String NEWAI = "newai";
}
```

## 步骤 1：Service 开发

**继承关系**: `CommonModelService` → `AbstractLLMService` → 你的 Service

**必须实现的抽象方法**：

| 方法 | 返回值 | 说明 |
|------|--------|------|
| `isEnabled()` | boolean | 检查服务是否可用（通常检查 API Key 是否配置） |
| `doBuildChatModel(ChatModelBuilderProperties)` | ChatModel | 构建同步对话模型 |
| `buildStreamingChatModel(ChatModelBuilderProperties)` | StreamingChatModel | 构建流式对话模型 |
| `parseError(Object)` | LLMException | 解析 API 错误信息 |
| `getTokenEstimator()` | TokenCountEstimator | Token 估算器（可返回 null） |

**可选覆盖的方法**：

| 方法 | 说明 |
|------|------|
| `checkBeforeChat(SseAskParams)` | 聊天前校验（如 DashScope 检查 base URL） |
| `doCreateChatRequestParameters(ChatRequestParameters, Map)` | 自定义请求参数（如开启深度思考、联网搜索） |

### 复杂度说明

基类 `AbstractLLMService`（约 600 行）已实现完整的对话流程（流式输出、工具调用、记忆管理、Token 统计、TTS 集成等），子类的实际工作量取决于目标平台：

- **langchain4j 已内置的平台**（如 Ollama）：直接使用对应的 `OllamaChatModel`，实现很简单（参考 `OllamaLLMService`，约 53 行）
- **有特殊参数的平台**：除了构建 Model，还需要覆盖 `doCreateChatRequestParameters()` 处理平台特有的参数（参考 `DashScopeLLMService` 约 131 行，处理深度思考、联网搜索等）

### 示例（参考 DashScopeLLMService）

非兼容平台需要使用平台方提供的 SDK 构建 Model：

```java
public class NewAiLLMService extends AbstractLLMService {
    @Override
    protected ChatModel doBuildChatModel(ChatModelBuilderProperties properties) {
        return NewAiSdkChatModel.builder()
                .baseUrl(platform.getBaseUrl())
                .apiKey(platform.getApiKey())
                .modelName(aiModel.getName())
                .temperature(properties.getTemperature().floatValue())
                .build();
    }
    // ... 其他方法类似
}
```

## 步骤 2：注册到 AiModelInitializer

**文件**: `adi-common/src/main/java/com/moyz/adi/common/service/AiModelInitializer.java`

在 `initLLMServiceList()` 方法中添加：

```java
private synchronized void initLLMServiceList(Map<String, ModelPlatform> nameToPlatform, String modelType) {
    // ... 现有平台 ...

    // 新增
    initLLMService(AdiConstant.ModelPlatform.NEWAI, modelType,
        model -> new NewAiLLMService(model, nameToPlatform.get(AdiConstant.ModelPlatform.NEWAI))
            .setProxyAddress(proxyAddress));
}
```
