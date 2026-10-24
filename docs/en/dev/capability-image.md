# Image Generation Capability Integration

> [← Developer Docs](index.md) · [简体中文](../../cn/dev/capability-image.md)

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

- **Platforms already supported by langchain4j** (e.g. OpenAI): Use `OpenAiImageModel` directly — very simple (see `OpenAiImageService`; it dispatches t2i vs i2i: without reference images it uses the built-in model, with them the custom `OpenAiImageEditModel` calling `/images/edits`)
- **Platforms requiring custom adaptation**: You need to write a custom `ImageModel` implementation to interface with the platform API, then use it in the Service (see `DashScopeImageService`; t2i and i2i share one endpoint, reference images are relayed as base64 data URIs via `AdiImageGenerationModel`)

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

The frontend uses a single unified form component `GenerateImageForm.vue`; form fields (size/count/quality/seed/reference images) are all driven by the model's `properties`, so new platforms usually need no new frontend component.

### Frontend Component Structure

```
src/views/draw/components/
  ├── GenerateImageForm.vue      — Unified generation form (shared by all platforms; fields rendered per model properties)
  ├── RefImageUpload.vue         — Reference-image upload for i2i (shown for models whose input_types include image)
  ├── ParamsPanel.vue            — Parameter section collapse/expand (floating arrow)
  ├── CommonDraws.vue            — Common draw operations
  ├── Header.vue                 — Page header (with model selector)
  └── SearchInput.vue            — Prompt input
```

### What to Develop

**Usually no frontend work**: all platforms share the unified `GenerateImageForm.vue`, whose fields (size/count/quality/seed/reference images) render from the model's `adi_ai_model.properties` declarations. Integrating a new platform only requires configuring the model in the admin console with (all five keys are declared uniformly; empty/false/1 hides the corresponding form field):

| properties key | Purpose | When empty |
|------|------|---------|
| `sizes` | Size options (`[{value,label}]`) | Empty array falls back to defaults |
| `max_images` | Per-run image count cap (slider shown when > 1) | 1 (no slider) |
| `qualities` | Quality options (e.g. `["auto","low","medium","high"]`) | No quality field |
| `is_support_seed` | Whether the seed field is shown | `false` hides the seed field |
| `max_ref_images` | Reference-image limit (paired with `image` in `input_types`) | 1 |

The reference-image upload appears automatically for models whose `input_types` include `image` — no frontend change needed.

---

Previous: [Chat / Vision Integration](capability-chat.md) · Next: [TTS Integration](capability-tts.md)
