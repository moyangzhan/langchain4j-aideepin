# 文生图能力接入

> [← 开发文档](index.md) · [English](../../en/dev/capability-image.md)

## 是否需要写代码？

**始终需要**。文生图接口没有行业统一的 API 标准，各厂商使用不同的私有 API 格式，因此无论平台是否兼容 OpenAI 对话接口，都需要编写 Service 类。

## 前置步骤：注册平台名称常量

在 `AdiConstant.ModelPlatform` 中添加新平台名称常量：

**文件**: `adi-common/src/main/java/com/moyz/adi/common/cosntant/AdiConstant.java`

```java
public static class ModelPlatform {
    // ... 现有常量 ...
    public static final String NEWAI = "newai";
}
```

## 步骤 1：Service 开发

**继承关系**: `CommonModelService` → `AbstractImageModelService` → 你的 Service

基类 `AbstractImageModelService` 已实现完整的图片生成流程（调用模型、处理 URL/Base64 响应、保存文件），子类只需构建 `ImageModel` 实例并处理错误。

**必须实现的抽象方法**：

| 方法 | 返回值 | 说明 |
|------|--------|------|
| `isEnabled()` | boolean | 检查服务是否可用 |
| `buildImageModel(User, Draw)` | ImageModel | 构建图片生成模型 |
| `parseError(Object)` | LLMException | 解析 API 错误 |

### 复杂度说明

实际开发复杂度取决于目标平台 API 的差异程度：

- **langchain4j 已内置的平台**（如 OpenAI）：直接使用 `OpenAiImageModel`，实现非常简单（参考 `OpenAiImageService`；文生图与图生图分派：无参考图走内置模型，有参考图走自研 `OpenAiImageEditModel` 调 `/images/edits`）
- **需要自定义适配的平台**：需要编写自定义的 `ImageModel` 实现来对接平台 API，再在 Service 中使用（参考 `DashScopeImageService`；文生图与图生图同端点，参考图以 base64 data URI 经 `AdiImageGenerationModel` 直传）

## 步骤 2：注册到 AiModelInitializer

**文件**: `adi-common/src/main/java/com/moyz/adi/common/service/AiModelInitializer.java`

在 `initImageModelServiceList()` 方法中添加：

```java
private synchronized void initImageModelServiceList(Map<String, ModelPlatform> nameToPlatform) {
    // ... 现有平台 ...

    // 新增
    initImageModelService(AdiConstant.ModelPlatform.NEWAI,
        model -> new NewAiImageService(model, nameToPlatform.get(AdiConstant.ModelPlatform.NEWAI)));
}
```

## 步骤 3：前端适配（用户端）

**文件**: `langchain4j-aideepin-web/src/views/draw/`

前端使用单一通用表单组件 `GenerateImageForm.vue`，表单项（尺寸/数量/质量/种子/参考图）全部由模型 `properties` 配置驱动，新平台接入通常无需新建前端组件。

### 前端组件结构

```
src/views/draw/components/
  ├── GenerateImageForm.vue      — 通用生成表单（所有平台共用，表单项随模型 properties 动态渲染）
  ├── RefImageUpload.vue         — 图生图参考图上传（input_types 含 image 的模型展示）
  ├── ParamsPanel.vue            — 参数区收起/展开（悬浮箭头）
  ├── CommonDraws.vue            — 通用绘图操作
  ├── Header.vue                 — 页面头（含模型选择器）
  └── SearchInput.vue            — 提示词输入框
```

### 需要开发的内容

**前端通常无需开发**：所有平台共用 `GenerateImageForm.vue` 通用表单，表单项（尺寸/数量/质量/种子/参考图）全部由模型的 `adi_ai_model.properties` 声明驱动渲染。新平台接入只需在管理后台配置模型并填写以下声明（五个字段统一声明齐全，空数组/false/1 表示对应表单项不展示）：

| properties 字段 | 作用 | 取空值时 |
|------|------|---------|
| `sizes` | 尺寸档位（`[{value,label}]`） | 空数组时回退默认档位 |
| `max_images` | 单次生成张数上限（>1 时展示数量滑杆） | 1（不展示滑杆） |
| `qualities` | 质量档位（如 `["auto","low","medium","high"]`） | 不展示质量项 |
| `is_support_seed` | 是否展示随机种子 | `false` 不展示种子项 |
| `max_ref_images` | 图生图参考图数量上限（配合 `input_types` 含 `image` 声明） | 1 |

参考图上传由 `input_types` 含 `image` 自动展示，无需前端改动。

---

上一篇：[对话 / 图像识别能力接入](capability-chat.md) ｜ 下一篇：[语音合成（TTS）能力接入](capability-tts.md)
