# Speech-to-Text (ASR) Capability Integration

## Is Code Required?

**Always required**. The ASR API has no industry-standard format. Each vendor uses different proprietary API formats, so regardless of whether the platform is OpenAI-compatible for chat, you need to write a Service class.

> **Note**: The ASR service is specified by system-level configuration; users cannot switch between providers. Only one ASR service is active at a time. The system determines the active ASR service via the `adi_sys_config` table record where `name = 'asr_setting'`. The `value` is in JSON format:
>
> ```json
> {"model_name": "new-model-name", "platform": "newai", "max_record_duration": 60, "max_file_size": 10485760}
> ```
>
> You can change this via the admin backend, or update the database directly:
>
> ```sql
> UPDATE adi_sys_config SET value = '{"model_name":"newai-asr-v1","platform":"newai","max_record_duration":60,"max_file_size":10485760}' WHERE name = 'asr_setting';
> ```

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

**Inheritance**: `CommonModelService` → `AbstractAsrModelService` → Your Service

The base class `AbstractAsrModelService` is thin (~20 lines), defining only the `audioToText` abstract method and proxy settings, so most of the logic must be implemented by subclasses.

**Required abstract methods**:

| Method | Return Type | Description |
|--------|-------------|-------------|
| `audioToText(String urlOrPath)` | String | Receive an audio file URL or local path, return recognized text |

### Complexity Notes

Implementation complexity is moderate. It requires handling file uploads (local or remote URLs), HTTP requests, response parsing, etc.:

- **Async API mode** (see `DashScopeAsrService`, ~76 lines): Uses SDK to submit an async task, polls for results. Supports remote URLs only
- **Sync upload mode** (see `SiliconflowAsrService`, ~90 lines): Uses multipart/form-data to upload audio files, returns results synchronously. Supports both local files and remote URLs

## Step 2: Register in AiModelInitializer

**File**: `adi-common/src/main/java/com/moyz/adi/common/service/AiModelInitializer.java`

Add in the `initAsrModelServiceList()` method:

```java
private synchronized void initAsrModelServiceList(Map<String, ModelPlatform> nameToPlatform) {
    // ... existing platforms ...

    // Add new
    initAsrModelService(AdiConstant.ModelPlatform.NEWAI,
        model -> new NewAiAsrService(model, nameToPlatform.get(AdiConstant.ModelPlatform.NEWAI)));
}
```
