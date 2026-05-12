# 语音识别（ASR）能力接入

## 是否需要写代码？

**始终需要**。ASR 接口没有行业统一的 API 标准，各厂商使用不同的私有 API 格式，因此无论平台是否兼容 OpenAI 对话接口，都需要编写 Service 类。

> **注意**：ASR 服务由系统全局配置指定，用户不能自行切换。同一时间系统只有一个 ASR 服务处于激活状态。系统通过 `adi_sys_config` 表中 `name = 'asr_setting'` 的记录指定激活的 ASR 服务，其 `value` 为 JSON 格式：
>
> ```json
> {"model_name": "新平台的模型名称", "platform": "newai", "max_record_duration": 60, "max_file_size": 10485760}
> ```
>
> 可通过管理后台修改此配置，也可以直接更新数据库：
>
> ```sql
> UPDATE adi_sys_config SET value = '{"model_name":"newai-asr-v1","platform":"newai","max_record_duration":60,"max_file_size":10485760}' WHERE name = 'asr_setting';
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

**继承关系**: `CommonModelService` → `AbstractAsrModelService` → 你的 Service

基类 `AbstractAsrModelService` 很薄（约 20 行），仅定义了 `audioToText` 抽象方法和代理设置，因此大部分逻辑需要子类自行实现。

**必须实现的抽象方法**：

| 方法 | 返回值 | 说明 |
|------|--------|------|
| `audioToText(String urlOrPath)` | String | 接收音频文件 URL 或本地路径，返回识别的文本 |

### 复杂度说明

实现复杂度中等，需要处理文件上传（本地文件或远程 URL）、HTTP 请求、响应解析等：

- **异步 API 模式**（参考 `DashScopeAsrService`，约 76 行）：使用 SDK 提交异步任务，轮询等待结果，仅支持远程 URL
- **同步上传模式**（参考 `SiliconflowAsrService`，约 90 行）：使用 multipart/form-data 上传音频文件，同步返回结果，支持本地文件和远程 URL

## 步骤 2：注册到 AiModelInitializer

**文件**: `adi-common/src/main/java/com/moyz/adi/common/service/AiModelInitializer.java`

在 `initAsrModelServiceList()` 方法中添加：

```java
private synchronized void initAsrModelServiceList(Map<String, ModelPlatform> nameToPlatform) {
    // ... 现有平台 ...

    // 新增
    initAsrModelService(AdiConstant.ModelPlatform.NEWAI,
        model -> new NewAiAsrService(model, nameToPlatform.get(AdiConstant.ModelPlatform.NEWAI)));
}
```
