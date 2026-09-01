# Image Generation Capability Integration

## Is Code Required?

**Always required**. The image generation API has no industry-standard format. Each vendor uses different proprietary API formats, so regardless of whether the platform is OpenAI-compatible for chat, you need to write a Service class.

## Prerequisite: Register Platform Name Constant

Add a new platform name constant in `AdiConstant.ModelPlatform`:

**File**: `adi-common/src/main/java/com/moyz/adi/common/cosntant/AdiConstant.java`

```java
public static class ModelPlatform {
    // ... existing constants ...
    public static final String NEWAI = "newai";
}
```

## Step 1: Service Development

**Inheritance**: `CommonModelService` → `AbstractImageModelService` → Your Service

The base class `AbstractImageModelService` implements the complete image generation flow (calling the model, handling URL/Base64 responses, saving files). Subclasses only need to provide an `ImageModel` instance and handle errors.

**Required abstract methods**:

| Method | Return Type | Description |
|--------|-------------|-------------|
| `isEnabled()` | boolean | Check if the service is available |
| `buildImageModel(User, Draw)` | ImageModel | Build the image generation model |
| `parseError(Object)` | LLMException | Parse API errors |

### Complexity Notes

The actual development complexity depends on how different the target platform's API is:

- **Platforms already supported by langchain4j** (e.g. OpenAI): Use `OpenAiImageModel` directly — very simple (see `OpenAiImageService`, ~22 lines)
- **Platforms requiring custom adaptation**: You need to write a custom `ImageModel` implementation to interface with the platform API, then use it in the Service (see `DashScopeWanxService`, ~74 lines, handling special modes like background generation)

## Step 2: Register in AiModelInitializer

**File**: `adi-common/src/main/java/com/moyz/adi/common/service/AiModelInitializer.java`

Add in the `initImageModelServiceList()` method:

```java
private synchronized void initImageModelServiceList(Map<String, ModelPlatform> nameToPlatform) {
    // ... existing platforms ...

    // Add new
    initImageModelService(AdiConstant.ModelPlatform.NEWAI,
        model -> new NewAiImageService(model, nameToPlatform.get(AdiConstant.ModelPlatform.NEWAI)));
}
```

## Step 3: Frontend Adaptation (User Web)

**File**: `langchain4j-aideepin-web/src/views/draw/`

The frontend has platform-specific generation components. You need to create a corresponding component for the new platform.

### Frontend Component Structure

```
src/views/draw/components/
  ├── gpt-image/
  │   └── GptImageEditor.vue    — OpenAI image editor
  ├── wanx/
  │   ├── index.vue              — DashScope Wanx entry
  │   ├── GenerateImage.vue      — Image generation
  │   └── GenerateBackground.vue — Background generation
  ├── siliconflow/
  │   ├── index.vue              — SiliconFlow entry
  │   └── GenerateImage.vue      — Image generation
  ├── CommonDraws.vue            — Common draw operations
  ├── Header.vue                 — Page header (with model selector)
  └── SearchInput.vue            — Prompt input
```

### What to Develop

1. **Create platform component directory**: Create `newai/` under `src/views/draw/components/` with `index.vue` and `GenerateImage.vue`

2. **Implement generation component**: Reference existing components (e.g. `siliconflow/GenerateImage.vue`), implement the following:
   - Call `api.imageGenerate()` with `modelName`, `prompt`, `size`, `number`, etc.
   - Call `checkProcess(uuid)` to poll for generation results
   - Push results into `drawStore`

3. **Register platform component**: Import the new component in `src/views/draw/index.vue` and render it based on the selected image model platform

### Parameter Differences by Platform

| Parameter | OpenAI | DashScope Wanx | SiliconFlow |
|-----------|--------|---------------|-------------|
| Image Size | auto/1024x1024/1024x1536/1536x1024 | Fixed options | Dynamically read from model `properties.image_sizes` |
| Image Quality | auto/low/medium/high | — | — |
| Generation Count | 1 | 1 | 1-4 |
| Random Seed | — | -1 (random) | -1 (random) |
| Negative Prompt | — | — | — |

### Image Model Data Requirements

The frontend reads the currently selected image model via `appStore.selectedImageModel`, which comes from the `adi_ai_model` table. If the new platform needs the frontend to dynamically read configuration (e.g. SiliconFlow reads size options from `properties.image_sizes`), configure the corresponding fields in `adi_ai_model.properties`.
