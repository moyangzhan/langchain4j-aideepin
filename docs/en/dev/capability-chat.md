# Chat / Vision Capability Integration

Chat (`text`) and Vision (`vision`) share the same LLM infrastructure and use the same integration logic.

## Is Code Required?

| Platform Compatible with OpenAI API | Platform Not Compatible |
|:----------------------------------:|:-----------------------:|
| No | **Yes** |

- **OpenAI API compatible platforms**: Simply set `is_openai_api_compatible` to `true` in `adi_model_platform`. The system will automatically load it via `OpenAiCompatibleLLMService` on startup — **zero code**.
- **Non-compatible platforms**: You need to write a Service class. See below.

## Prerequisite: Register Platform Name Constant

Add a new platform name constant in `AdiConstant.ModelPlatform`:

**File**: `adi-common/src/main/java/com/moyz/adi/common/cosntant/AdiConstant.java`

```java
public static class ModelPlatform {
    public static final String DEEPSEEK = "deepseek";
    public static final String OPENAI = "openai";
    public static final String DASHSCOPE = "dashscope";
    public static final String OLLAMA = "ollama";
    public static final String SILICONFLOW = "siliconflow";
    // Add new
    public static final String NEWAI = "newai";
}
```

## Step 1: Service Development

**Inheritance**: `CommonModelService` → `AbstractLLMService` → Your Service

**Required abstract methods**:

| Method | Return Type | Description |
|--------|-------------|-------------|
| `isEnabled()` | boolean | Check if the service is available (typically checks if API Key is configured) |
| `doBuildChatModel(ChatModelBuilderProperties)` | ChatModel | Build a synchronous chat model |
| `buildStreamingChatModel(ChatModelBuilderProperties)` | StreamingChatModel | Build a streaming chat model |
| `parseError(Object)` | LLMException | Parse API error information |
| `getTokenEstimator()` | TokenCountEstimator | Token estimator (can return null) |

**Optional methods to override**:

| Method | Description |
|--------|-------------|
| `checkBeforeChat(SseAskParams)` | Pre-chat validation (e.g. DashScope checks base URL) |
| `doCreateChatRequestParameters(ChatRequestParameters, Map)` | Customize request parameters (e.g. enable deep thinking, web search) |

### Complexity Notes

The base class `AbstractLLMService` (~600 lines) implements the complete chat flow (streaming output, tool calling, memory management, token statistics, TTS integration, etc.). The actual effort depends on the target platform:

- **Platforms already supported by langchain4j** (e.g. Ollama): Use the corresponding `OllamaChatModel` directly — very simple (see `OllamaLLMService`, ~53 lines)
- **Platforms with special parameters**: In addition to building the Model, you also need to override `doCreateChatRequestParameters()` to handle platform-specific parameters (see `DashScopeLLMService`, ~131 lines, handling deep thinking, web search, etc.)

### Example (reference DashScopeLLMService)

Non-compatible platforms need to use the platform provider's SDK to build the Model:

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
    // ... other methods similar
}
```

## Step 2: Register in AiModelInitializer

**File**: `adi-common/src/main/java/com/moyz/adi/common/service/AiModelInitializer.java`

Add in the `initLLMServiceList()` method:

```java
private synchronized void initLLMServiceList(Map<String, ModelPlatform> nameToPlatform, String modelType) {
    // ... existing platforms ...

    // Add new
    initLLMService(AdiConstant.ModelPlatform.NEWAI, modelType,
        model -> new NewAiLLMService(model, nameToPlatform.get(AdiConstant.ModelPlatform.NEWAI))
            .setProxyAddress(proxyAddress));
}
```
