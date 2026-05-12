# 文生图能力接入

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

- **langchain4j 已内置的平台**（如 OpenAI）：直接使用 `OpenAiImageModel`，实现非常简单（参考 `OpenAiImageService`，约 22 行）
- **需要自定义适配的平台**：需要编写自定义的 `ImageModel` 实现来对接平台 API，再在 Service 中使用（参考 `DashScopeWanxService`，约 74 行，需处理背景生成等特殊模式）

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

前端按平台划分了独立的生成组件，需要为新平台创建对应的组件。

### 前端组件结构

```
src/views/draw/components/
  ├── gpt-image/
  │   └── GptImageEditor.vue    — OpenAI 图像编辑器
  ├── wanx/
  │   ├── index.vue              — DashScope 万相入口
  │   ├── GenerateImage.vue      — 图片生成
  │   └── GenerateBackground.vue — 背景生成
  ├── siliconflow/
  │   ├── index.vue              — SiliconFlow 入口
  │   └── GenerateImage.vue      — 图片生成
  ├── CommonDraws.vue            — 通用绘图操作
  ├── Header.vue                 — 页面头（含模型选择器）
  └── SearchInput.vue            — 提示词输入框
```

### 需要开发的内容

1. **创建平台组件目录**：在 `src/views/draw/components/` 下新建 `newai/` 目录，创建 `index.vue` 和 `GenerateImage.vue`

2. **实现生成组件**：参考现有组件（如 `siliconflow/GenerateImage.vue`），实现以下逻辑：
   - 调用 `api.imageGenerate()` 接口，传入 `modelName`、`prompt`、`size`、`number` 等参数
   - 调用 `checkProcess(uuid)` 轮询生成结果
   - 将结果推入 `drawStore`

3. **注册平台组件**：在 `src/views/draw/index.vue` 中引入新组件，根据选中的图像模型平台动态渲染对应组件

### 各平台参数差异

| 参数 | OpenAI | DashScope 万相 | SiliconFlow |
|------|--------|---------------|-------------|
| 图片尺寸 | auto/1024x1024/1024x1536/1536x1024 | 固定选项 | 从模型 `properties.image_sizes` 动态读取 |
| 图片质量 | auto/low/medium/high | — | — |
| 生成数量 | 1 | 1 | 1-4 |
| 随机种子 | — | -1（随机） | -1（随机） |
| 负向提示词 | — | — | — |

### 图像模型数据要求

前端通过 `appStore.selectedImageModel` 获取当前选中的图像模型信息，该数据来源于 `adi_ai_model` 表。如果新平台需要前端动态读取配置（如 SiliconFlow 从 `properties.image_sizes` 读取尺寸选项），需要在 `adi_ai_model.properties` 中配置对应字段。
