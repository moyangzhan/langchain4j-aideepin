# 语音合成（TTS）能力接入

## 是否需要写代码？

**始终需要**。TTS 接口没有行业统一的 API 标准，各厂商使用不同的私有 API 格式，因此无论平台是否兼容 OpenAI 对话接口，都需要编写 Service 类。

> **注意**：TTS 服务由系统全局配置指定，用户不能自行切换。同一时间系统只有一个 TTS 服务处于激活状态。系统通过 `adi_sys_config` 表中 `name = 'tts_setting'` 的记录指定激活的 TTS 服务，其 `value` 为 JSON 格式：
>
> ```json
> {"synthesizer_side": "server", "model_name": "新平台的模型名称", "platform": "newai"}
> ```
>
> 可通过管理后台修改此配置，也可以直接更新数据库：
>
> ```sql
> UPDATE adi_sys_config SET value = '{"synthesizer_side":"server","model_name":"newai-tts-v1","platform":"newai"}' WHERE name = 'tts_setting';
> ```

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

**继承关系**: `CommonModelService` → `AbstractTtsModelService` → 你的 Service

基类 `AbstractTtsModelService` 很薄（约 30 行），仅定义了三个抽象方法的契约和代理设置，因此大部分逻辑需要子类自行实现。

**必须实现的抽象方法**：

| 方法 | 说明 |
|------|------|
| `start(String jobId, String voice, Consumer<ByteBuffer> onProcess, Consumer<String> onComplete, Consumer<String> onError)` | 初始化 TTS 任务，设置音色和回调 |
| `processByStream(String jobId, String partText)` | 流式接收文本片段并发送给 TTS 引擎 |
| `complete(String jobId)` | 完成所有文本输入，等待音频生成完成 |

### 复杂度说明

TTS 是所有能力中实现复杂度最高的。需要处理音频流、WAV 文件头生成、临时文件管理等底层细节：

- **流式 API 模式**（参考 `DashScopeTtsService`，约 215 行）：使用 SDK 实时接收音频帧，处理 PCM 数据并转 WAV 格式，涉及音频缓冲区操作和回调机制
- **REST API 模式**（参考 `SiliconflowTtsService`，约 154 行）：先积累文本，完成后一次性发送请求，再处理流式音频响应并写入文件

## 步骤 2：注册到 AiModelInitializer

**文件**: `adi-common/src/main/java/com/moyz/adi/common/service/AiModelInitializer.java`

在 `initTtsModelServiceList()` 方法中添加：

```java
private synchronized void initTtsModelServiceList(Map<String, ModelPlatform> nameToPlatform) {
    // ... 现有平台 ...

    // 新增
    initTtsModelService(AdiConstant.ModelPlatform.NEWAI,
        model -> new NewAiTtsService(model, nameToPlatform.get(AdiConstant.ModelPlatform.NEWAI)));
}
```
