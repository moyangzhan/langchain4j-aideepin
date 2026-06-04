# New Model Platform Integration Guide

## 1. Overview

AIDeepIn uses **Strategy Pattern + Factory Pattern + Context Pattern** to manage multiple model platforms. The system currently supports the following capability types:

| Type | Constant | Description | Integration Doc |
|------|----------|-------------|----------------|
| Chat | `text` | Text conversation (Chat Completions) | [Chat Integration](capability-chat.md) |
| Vision | `vision` | Multimodal conversation (supports image input) | [Chat Integration](capability-chat.md) |
| Image Generation | `image` | Text-to-image generation | [Image Integration](capability-image.md) |
| Text-to-Speech | `tts` | Text to speech | [TTS Integration](capability-tts.md) |
| Speech-to-Text | `asr` | Speech to text | [ASR Integration](capability-asr.md) |

> **Note**: Chat (`text`) and Vision (`vision`) share the same LLM infrastructure (`AbstractLLMService`) and use the same integration logic.

Integrating a new platform involves two parts:
1. **Part 1: Database Configuration (Required)** — See Section 2 below
2. **Part 2: Code Development (As Needed)** — Read the corresponding capability integration doc based on which capabilities you want to enable

> **Quick Start**: If the new platform is compatible with the OpenAI API and you only need Chat (`text`) or Vision (`vision`) functionality, simply complete the database configuration — no code required. The system will automatically load the platform's chat models via `OpenAiCompatibleLLMService`.

---

## 2. Database Configuration (Required)

### 2.1 Model Platform Table (adi_model_platform)

This table stores platform-level configuration (API endpoint, credentials, etc.).

```sql
INSERT INTO adi_model_platform (name, title, base_url, api_key, remark, is_proxy_enable, is_openai_api_compatible)
VALUES ('newai', 'NewAI', 'https://api.newai.com/v1', 'your-api-key', 'NewAI model platform', false, true);
```

| Field | Type | Description |
|-------|------|-------------|
| `name` | varchar(45) | Platform unique identifier, e.g. `openai`, `dashscope`, `ollama`. **Must match the constant in `AdiConstant.ModelPlatform`** |
| `title` | varchar(45) | Display name, e.g. `OpenAI`, `DeepSeek` |
| `base_url` | varchar(250) | API base URL, e.g. `https://api.openai.com/v1` |
| `api_key` | varchar(100) | API key |
| `secret_key` | varchar(100) | Deprecated, kept for compatibility only |
| `remark` | varchar(1000) | Remarks |
| `is_proxy_enable` | boolean | Whether to access via proxy (proxy is configured in `application.yml` under `adi.proxy`) |
| `is_openai_api_compatible` | boolean | **Key field**: Whether the platform is compatible with the OpenAI API format. When set to `true`, chat (text/vision) models will be automatically loaded using `OpenAiCompatibleLLMService` with no code required |

### 2.2 AI Model Table (adi_ai_model)

This table stores specific model information under a platform. **Each capability type corresponds to one record**.

```sql
-- Chat model
INSERT INTO adi_ai_model (name, title, type, platform, context_window, max_input_tokens, max_output_tokens,
    input_types, response_format_types, is_free, is_enable, properties)
VALUES ('newai-chat-v1', 'NewAI Chat V1', 'text', 'newai', 128000, 120000, 8000,
    'text', 'text,json_object', false, true, '{}');

-- Vision model
INSERT INTO adi_ai_model (name, title, type, platform, context_window, max_input_tokens, max_output_tokens,
    input_types, response_format_types, is_free, is_enable, properties)
VALUES ('newai-vision-v1', 'NewAI Vision V1', 'vision', 'newai', 128000, 120000, 8000,
    'text,image', 'text', false, true, '{}');

-- Image generation model
INSERT INTO adi_ai_model (name, title, type, platform, context_window, max_input_tokens, max_output_tokens,
    input_types, response_format_types, is_free, is_enable, properties)
VALUES ('newai-image-v1', 'NewAI Image V1', 'image', 'newai', 0, 0, 0,
    'text', 'text', false, true, '{}');

-- TTS model
INSERT INTO adi_ai_model (name, title, type, platform, context_window, max_input_tokens, max_output_tokens,
    input_types, response_format_types, is_free, is_enable, properties)
VALUES ('newai-tts-v1', 'NewAI TTS V1', 'tts', 'newai', 0, 0, 0,
    'text', 'text', false, true, '{"voices":["alloy","echo","fable"]}');

-- ASR model
INSERT INTO adi_ai_model (name, title, type, platform, context_window, max_input_tokens, max_output_tokens,
    input_types, response_format_types, is_free, is_enable, properties)
VALUES ('newai-asr-v1', 'NewAI ASR V1', 'asr', 'newai', 0, 0, 0,
    'audio', 'text', false, true, '{}');
```

| Field | Type | Description |
|-------|------|-------------|
| `name` | varchar(45) | Model name, **must exactly match the model name specified by the platform provider** (passed as API parameter) |
| `title` | varchar(45) | Display name |
| `type` | varchar(45) | Model type: `text`, `image`, `vision`, `embedding`, `rerank`, `tts`, `asr` |
| `platform` | varchar(45) | Corresponds to `adi_model_platform.name` |
| `context_window` | int | Context window size (token count) |
| `max_input_tokens` | int | Maximum input token count |
| `max_output_tokens` | int | Maximum output token count |
| `input_types` | varchar(100) | Supported input types: `text`, `image`, `audio`, `video`, comma-separated |
| `response_format_types` | varchar(200) | Supported output formats: `text`, `json_object`, comma-separated |
| `properties` | jsonb | Model-specific properties, e.g. TTS voice list `{"voices":["v1","v2"]}` |
| `setting` | varchar(500) | Model configuration, JSON format |
| `is_free` | boolean | Whether the model is free |
| `is_enable` | boolean | Whether the model is enabled |
| `is_reasoner` | boolean | Whether the model is a reasoning model (e.g. deepseek-r1) |
| `is_thinking_closable` | boolean | Whether the thinking process can be disabled (e.g. Qwen3 can, deepseek-r1 cannot) |
| `is_support_web_search` | boolean | Whether the model supports web search |

### 2.3 Using the Admin API

Alternatively, you can use the admin backend API instead of executing SQL directly:

- `POST /admin/model-platform/add` — Add platform
- `POST /admin/model-platform/edit` — Edit platform
- `POST /admin/model/addOne` — Add model
- `POST /admin/model/edit` — Edit model

### 2.4 Worked Example: Atlas Cloud (OpenAI-compatible)

[Atlas Cloud](https://www.atlascloud.ai/?utm_source=github&utm_medium=link&utm_campaign=langchain4j-aideepin) is a full-modal inference platform with an OpenAI-compatible API, so it integrates with **database configuration only — no code** (`is_openai_api_compatible = true`, auto-loaded via `OpenAiCompatibleLLMService`).

```sql
-- 1) Platform
INSERT INTO adi_model_platform (name, title, base_url, api_key, remark, is_proxy_enable, is_openai_api_compatible)
VALUES ('atlascloud', 'Atlas Cloud', 'https://api.atlascloud.ai/v1', 'your-atlascloud-api-key', 'Atlas Cloud (OpenAI-compatible)', false, true);

-- 2) Chat models (type=text). Adjust token limits to each model's real values.
--    deepseek-v4-pro is a reasoning model -> is_reasoner = true.
INSERT INTO adi_ai_model (name, title, type, platform, context_window, max_input_tokens, max_output_tokens,
    input_types, response_format_types, is_free, is_enable, is_reasoner)
VALUES ('deepseek-ai/deepseek-v4-pro', 'DeepSeek V4 Pro (Atlas)', 'text', 'atlascloud', 128000, 120000, 8000,
    'text', 'text,json_object', false, true, true);

INSERT INTO adi_ai_model (name, title, type, platform, context_window, max_input_tokens, max_output_tokens,
    input_types, response_format_types, is_free, is_enable)
VALUES ('Qwen/Qwen3-Next-80B-A3B-Instruct', 'Qwen3 Next 80B Instruct (Atlas)', 'text', 'atlascloud', 128000, 120000, 8000,
    'text', 'text,json_object', false, true);
```

`adi_ai_model.name` must match the Atlas model id exactly (it is passed through as the API `model` parameter). Add any of the models below the same way:

<details>
<summary>All Atlas Cloud chat models (59)</summary>

- **Anthropic (Claude):** `anthropic/claude-haiku-4.5-20251001`, `anthropic/claude-opus-4.8`, `anthropic/claude-sonnet-4.6`
- **OpenAI (GPT):** `openai/gpt-5.4`, `openai/gpt-5.5`
- **Google (Gemini):** `google/gemini-3.1-flash-lite`, `google/gemini-3.1-pro-preview`, `google/gemini-3.5-flash`
- **Qwen:** `qwen/qwen2.5-7b-instruct`, `Qwen/Qwen3-235B-A22B-Instruct-2507`, `qwen/qwen3-235b-a22b-thinking-2507`, `qwen/qwen3-30b-a3b`, `Qwen/Qwen3-30B-A3B-Instruct-2507`, `qwen/qwen3-30b-a3b-thinking-2507`, `qwen/qwen3-32b`, `qwen/qwen3-8b`, `Qwen/Qwen3-Coder`, `qwen/qwen3-coder-next`, `qwen/qwen3-max-2026-01-23`, `Qwen/Qwen3-Next-80B-A3B-Instruct`, `Qwen/Qwen3-Next-80B-A3B-Thinking`, `Qwen/Qwen3-VL-235B-A22B-Instruct`, `qwen/qwen3-vl-235b-a22b-thinking`, `qwen/qwen3-vl-30b-a3b-instruct`, `qwen/qwen3-vl-30b-a3b-thinking`, `qwen/qwen3-vl-8b-instruct`, `qwen/qwen3.5-122b-a10b`, `qwen/qwen3.5-27b`, `qwen/qwen3.5-35b-a3b`, `qwen/qwen3.5-397b-a17b`, `qwen/qwen3.6-35b-a3b`, `qwen/qwen3.6-plus`
- **DeepSeek:** `deepseek-ai/deepseek-ocr`, `deepseek-ai/deepseek-r1-0528`, `deepseek-ai/DeepSeek-V3-0324`, `deepseek-ai/DeepSeek-V3.1`, `deepseek-ai/DeepSeek-V3.1-Terminus`, `deepseek-ai/deepseek-v3.2`, `deepseek-ai/DeepSeek-V3.2-Exp`, `deepseek-ai/deepseek-v4-flash`, `deepseek-ai/deepseek-v4-pro`
- **Moonshot (Kimi):** `moonshotai/Kimi-K2-Instruct`, `moonshotai/Kimi-K2-Instruct-0905`, `moonshotai/Kimi-K2-Thinking`, `moonshotai/kimi-k2.5`, `moonshotai/kimi-k2.6`
- **Zhipu (GLM):** `zai-org/GLM-4.6`, `zai-org/glm-4.7`, `zai-org/glm-5`, `zai-org/glm-5-turbo`, `zai-org/glm-5.1`, `zai-org/glm-5v-turbo`
- **MiniMax:** `MiniMaxAI/MiniMax-M2`, `minimaxai/minimax-m2.1`, `minimaxai/minimax-m2.5`, `minimaxai/minimax-m2.7`
- **xAI:** `xai/grok-4.3`
- **Kwaipilot:** `kwaipilot/kat-coder-pro-v2`
- **Other:** `owl`

</details>

---

## 3. Debugging & Verification

### Check Startup Logs

The service prints model loading logs on startup:

```
add openai api compatible llm model,model: AiModel(name=newai-chat-v1, ...)
add llm model,model: AiModel(name=newai-chat-v1, ...)
add image model,model: AiModel(name=newai-image-v1, ...)
add tts model,model: AiModel(name=newai-tts-v1, ...)
add asr model,model: AiModel(name=newai-asr-v1, ...)
```

If you see `{platform} service is disabled`, it means no models are enabled for that platform or the API Key is not configured.

### Common Issues

| Issue | Cause | Solution |
|-------|-------|----------|
| Platform not loaded | `is_deleted = true` in `adi_model_platform` | Ensure the record is not soft-deleted |
| Model not loaded | `is_enable = false` in `adi_ai_model` | Set `is_enable` to `true` |
| Chat model not working despite `is_openai_api_compatible = true` | `api_key` is empty | Configure a valid API Key |
| Non-OpenAI-compatible platform model not working | Not registered in `AiModelInitializer` | Add registration code per the corresponding capability integration doc |
| Proxy not working | Platform `is_proxy_enable = false` or global proxy disabled | Check `adi_model_platform.is_proxy_enable` and `adi.proxy.enable` in `application.yml` |

---

## 4. Source Code Index

| File Path | Description |
|-----------|-------------|
| `adi-common/.../entity/ModelPlatform.java` | Model platform entity |
| `adi-common/.../entity/AiModel.java` | AI model entity |
| `adi-common/.../cosntant/AdiConstant.java` | Platform name constants (`ModelPlatform` inner class) + model type constants (`ModelType` inner class) |
| `adi-common/.../languagemodel/CommonModelService.java` | Base class for all services (holds AiModel and ModelPlatform references) |
| `adi-common/.../languagemodel/AbstractLLMService.java` | Chat service abstract base class |
| `adi-common/.../languagemodel/AbstractImageModelService.java` | Image generation abstract base class |
| `adi-common/.../languagemodel/AbstractTtsModelService.java` | TTS abstract base class |
| `adi-common/.../languagemodel/AbstractAsrModelService.java` | ASR abstract base class |
| `adi-common/.../languagemodel/OpenAiCompatibleLLMService.java` | OpenAI-compatible platform chat service (auto-loaded) |
| `adi-common/.../service/AiModelInitializer.java` | Service registration factory (all platforms register here) |
| `adi-common/.../helper/LLMContext.java` | Chat service context (stores and looks up AbstractLLMService) |
| `adi-common/.../helper/ImageModelContext.java` | Image generation service context |
| `adi-common/.../helper/TtsModelContext.java` | TTS service context |
| `adi-common/.../helper/AsrModelContext.java` | ASR service context |
