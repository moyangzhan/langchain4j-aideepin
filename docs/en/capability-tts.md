# Text-to-Speech (TTS) Capability Integration

## Is Code Required?

**Always required**. The TTS API has no industry-standard format. Each vendor uses different proprietary API formats, so regardless of whether the platform is OpenAI-compatible for chat, you need to write a Service class.

> **Note**: The TTS service is specified by system-level configuration; users cannot switch between providers. Only one TTS service is active at a time. The system determines the active TTS service via the `adi_sys_config` table record where `name = 'tts_setting'`. The `value` is in JSON format:
>
> ```json
> {"synthesizer_side": "server", "model_name": "new-model-name", "platform": "newai"}
> ```
>
> You can change this via the admin backend, or update the database directly:
>
> ```sql
> UPDATE adi_sys_config SET value = '{"synthesizer_side":"server","model_name":"newai-tts-v1","platform":"newai"}' WHERE name = 'tts_setting';
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

**Inheritance**: `CommonModelService` → `AbstractTtsModelService` → Your Service

The base class `AbstractTtsModelService` is thin (~30 lines), defining only the contract for three abstract methods and proxy settings, so most of the logic must be implemented by subclasses.

**Required abstract methods**:

| Method | Description |
|--------|-------------|
| `start(String jobId, String voice, Consumer<ByteBuffer> onProcess, Consumer<String> onComplete, Consumer<String> onError)` | Initialize TTS task, set voice and callbacks |
| `processByStream(String jobId, String partText)` | Receive streaming text fragments and send to TTS engine |
| `complete(String jobId)` | Finish all text input and wait for audio generation to complete |

### Complexity Notes

TTS has the highest implementation complexity among all capabilities. It requires handling audio streams, WAV header generation, temporary file management, and other low-level details:

- **Streaming API mode** (see `DashScopeTtsService`, ~215 lines): Uses SDK to receive audio frames in real-time, processes PCM data and converts to WAV format, involves audio buffer operations and callback mechanisms
- **REST API mode** (see `SiliconflowTtsService`, ~154 lines): Accumulates text first, sends a single request on completion, then processes streaming audio response and writes to file

## Step 2: Register in AiModelInitializer

**File**: `adi-common/src/main/java/com/moyz/adi/common/service/AiModelInitializer.java`

Add in the `initTtsModelServiceList()` method:

```java
private synchronized void initTtsModelServiceList(Map<String, ModelPlatform> nameToPlatform) {
    // ... existing platforms ...

    // Add new
    initTtsModelService(AdiConstant.ModelPlatform.NEWAI,
        model -> new NewAiTtsService(model, nameToPlatform.get(AdiConstant.ModelPlatform.NEWAI)));
}
```
