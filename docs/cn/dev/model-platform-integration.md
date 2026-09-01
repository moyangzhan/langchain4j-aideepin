# 新模型平台接入指南

## 1. 概述

AIDeepIn 使用 **策略模式 + 工厂模式 + 上下文模式** 管理多个模型平台。系统当前支持以下能力类型：

| 类型 | 常量值 | 说明 | 接入文档 |
|------|--------|------|---------|
| 对话 | `text` | 文本对话（Chat Completions） | [对话接入](capability-chat.md) |
| 图像识别 | `vision` | 多模态对话（支持图片输入） | [对话接入](capability-chat.md) |
| 文生图 | `image` | 文本生成图片 | [文生图接入](capability-image.md) |
| 语音合成 | `tts` | 文本转语音 | [TTS 接入](capability-tts.md) |
| 语音识别 | `asr` | 语音转文本 | [ASR 接入](capability-asr.md) |

> **注意**：对话（text）和图像识别（vision）使用同一套 LLM 基础设施（`AbstractLLMService`），共享同一套接入逻辑。

接入新平台分两部分：
1. **第一部分：数据库配置（必须）** — 见下文第 2 节
2. **第二部分：代码开发（按需）** — 根据需要启用的能力，阅读对应的能力接入文档

> **快速接入**：如果新平台兼容 OpenAI API 且只需要使用对话（text）或图像识别（vision）功能，只需完成数据库配置即可，无需编写任何代码。系统会通过 `OpenAiCompatibleLLMService` 自动加载该平台的对话模型。

---

## 2. 数据库配置（必须）

### 2.1 模型平台表（adi_model_platform）

该表存储平台级别的配置信息（API 地址、密钥等）。

```sql
INSERT INTO adi_model_platform (name, title, base_url, api_key, remark, is_proxy_enable, is_openai_api_compatible)
VALUES ('newai', 'NewAI', 'https://api.newai.com/v1', 'your-api-key', 'NewAI model platform', false, true);
```

| 字段 | 类型 | 说明 |
|------|------|------|
| `name` | varchar(45) | 平台唯一标识，如 `openai`、`dashscope`、`ollama`。**必须与代码中 `AdiConstant.ModelPlatform` 的常量值一致** |
| `title` | varchar(45) | 显示名称，如 `OpenAI`、`DeepSeek` |
| `base_url` | varchar(250) | API 请求基础地址，如 `https://api.openai.com/v1` |
| `api_key` | varchar(100) | API 密钥 |
| `secret_key` | varchar(100) | 已废弃，仅做兼容保留 |
| `remark` | varchar(1000) | 备注说明 |
| `is_proxy_enable` | boolean | 是否通过代理访问（代理配置在 `application.yml` 的 `adi.proxy` 中） |
| `is_openai_api_compatible` | boolean | **关键字段**：是否兼容 OpenAI API 格式。设为 `true` 后，对话(text/vision)类型模型将自动使用 `OpenAiCompatibleLLMService` 加载，无需编写代码 |

### 2.2 AI 模型表（adi_ai_model）

该表存储平台下的具体模型信息。**每个能力类型对应一条记录**。

```sql
-- 对话模型
INSERT INTO adi_ai_model (name, title, type, platform, context_window, max_input_tokens, max_output_tokens,
    input_types, response_format_types, is_free, is_enable, properties)
VALUES ('newai-chat-v1', 'NewAI Chat V1', 'text', 'newai', 128000, 120000, 8000,
    'text', 'text,json_object', false, true, '{}');

-- 图像识别模型
INSERT INTO adi_ai_model (name, title, type, platform, context_window, max_input_tokens, max_output_tokens,
    input_types, response_format_types, is_free, is_enable, properties)
VALUES ('newai-vision-v1', 'NewAI Vision V1', 'vision', 'newai', 128000, 120000, 8000,
    'text,image', 'text', false, true, '{}');

-- 文生图模型
INSERT INTO adi_ai_model (name, title, type, platform, context_window, max_input_tokens, max_output_tokens,
    input_types, response_format_types, is_free, is_enable, properties)
VALUES ('newai-image-v1', 'NewAI Image V1', 'image', 'newai', 0, 0, 0,
    'text', 'text', false, true, '{}');

-- TTS 模型
INSERT INTO adi_ai_model (name, title, type, platform, context_window, max_input_tokens, max_output_tokens,
    input_types, response_format_types, is_free, is_enable, properties)
VALUES ('newai-tts-v1', 'NewAI TTS V1', 'tts', 'newai', 0, 0, 0,
    'text', 'text', false, true, '{"voices":["alloy","echo","fable"]}');

-- ASR 模型
INSERT INTO adi_ai_model (name, title, type, platform, context_window, max_input_tokens, max_output_tokens,
    input_types, response_format_types, is_free, is_enable, properties)
VALUES ('newai-asr-v1', 'NewAI ASR V1', 'asr', 'newai', 0, 0, 0,
    'audio', 'text', false, true, '{}');
```

| 字段 | 类型 | 说明 |
|------|------|------|
| `name` | varchar(45) | 模型名称，**必须与模型平台方指定的模型名称完全一致**（传给 API 的参数） |
| `title` | varchar(45) | 显示名称 |
| `type` | varchar(45) | 模型类型：`text`、`image`、`vision`、`embedding`、`rerank`、`tts`、`asr` |
| `platform` | varchar(45) | 对应 `adi_model_platform.name` |
| `context_window` | int | 上下文窗口大小（token 数） |
| `max_input_tokens` | int | 最大输入 token 数 |
| `max_output_tokens` | int | 最大输出 token 数 |
| `input_types` | varchar(100) | 支持的输入类型：`text`、`image`、`audio`、`video`，多值用逗号分隔 |
| `response_format_types` | varchar(200) | 支持的输出格式：`text`、`json_object`，多值用逗号分隔 |
| `properties` | jsonb | 模型特有属性，如 TTS 模型的音色列表 `{"voices":["v1","v2"]}` |
| `setting` | varchar(500) | 模型配置，JSON 格式 |
| `is_free` | boolean | 是否免费 |
| `is_enable` | boolean | 是否启用 |
| `is_reasoner` | boolean | 是否为推理模型（如 deepseek-r1） |
| `is_thinking_closable` | boolean | 思考过程是否可关闭（如 Qwen3 可关闭，deepseek-r1 不可关闭） |
| `is_support_web_search` | boolean | 是否支持联网搜索 |

### 2.3 通过 Admin API 操作

除了直接执行 SQL，也可以通过管理后台 API 操作：

- `POST /admin/model-platform/add` — 添加平台
- `POST /admin/model-platform/edit` — 编辑平台
- `POST /admin/model/addOne` — 添加模型
- `POST /admin/model/edit` — 编辑模型

---

## 3. 调试与验证

### 查看启动日志

服务启动时会打印模型加载日志：

```
add openai api compatible llm model,model: AiModel(name=newai-chat-v1, ...)
add llm model,model: AiModel(name=newai-chat-v1, ...)
add image model,model: AiModel(name=newai-image-v1, ...)
add tts model,model: AiModel(name=newai-tts-v1, ...)
add asr model,model: AiModel(name=newai-asr-v1, ...)
```

如果看到 `{platform} service is disabled`，说明该平台下没有已启用的模型或 API Key 未配置。

### 常见问题

| 问题 | 原因 | 解决方案 |
|------|------|---------|
| 平台未被加载 | `adi_model_platform` 中 `is_deleted = true` | 确认记录未被软删除 |
| 模型未被加载 | `adi_ai_model` 中 `is_enable = false` | 将 `is_enable` 设为 `true` |
| 对话模型不生效但 `is_openai_api_compatible = true` | `api_key` 为空 | 配置有效的 API Key |
| 非 OpenAI 兼容平台模型不生效 | 未在 `AiModelInitializer` 中注册 | 按对应能力接入文档添加注册代码 |
| 代理不生效 | 平台 `is_proxy_enable = false` 或全局代理未开启 | 检查 `adi_model_platform.is_proxy_enable` 和 `application.yml` 中的 `adi.proxy.enable` |

---

## 4. 源码文件索引

| 文件路径 | 作用 |
|---------|------|
| `adi-common/.../entity/ModelPlatform.java` | 模型平台实体 |
| `adi-common/.../entity/AiModel.java` | AI 模型实体 |
| `adi-common/.../cosntant/AdiConstant.java` | 平台名称常量（`ModelPlatform` 内部类）+ 模型类型常量（`ModelType` 内部类） |
| `adi-common/.../languagemodel/CommonModelService.java` | 所有 Service 的基类（持有 AiModel 和 ModelPlatform 引用） |
| `adi-common/.../languagemodel/AbstractLLMService.java` | 对话服务抽象基类 |
| `adi-common/.../languagemodel/AbstractImageModelService.java` | 文生图抽象基类 |
| `adi-common/.../languagemodel/AbstractTtsModelService.java` | TTS 抽象基类 |
| `adi-common/.../languagemodel/AbstractAsrModelService.java` | ASR 抽象基类 |
| `adi-common/.../languagemodel/OpenAiCompatibleLLMService.java` | OpenAI 兼容平台的对话服务（自动加载） |
| `adi-common/.../service/AiModelInitializer.java` | 服务注册工厂（所有平台在此注册） |
| `adi-common/.../helper/LLMContext.java` | 对话服务上下文（存储和查找 AbstractLLMService） |
| `adi-common/.../helper/ImageModelContext.java` | 文生图服务上下文 |
| `adi-common/.../helper/TtsModelContext.java` | TTS 服务上下文 |
| `adi-common/.../helper/AsrModelContext.java` | ASR 服务上下文 |
