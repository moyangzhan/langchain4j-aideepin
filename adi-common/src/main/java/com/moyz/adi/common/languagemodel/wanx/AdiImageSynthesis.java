package com.moyz.adi.common.languagemodel.wanx;

import com.alibaba.dashscope.aigc.imagesynthesis.ImageSynthesis;
import com.alibaba.dashscope.aigc.imagesynthesis.ImageSynthesisListResult;
import com.alibaba.dashscope.aigc.imagesynthesis.ImageSynthesisParam;
import com.alibaba.dashscope.aigc.imagesynthesis.ImageSynthesisResult;
import com.alibaba.dashscope.aigc.imagesynthesis.SketchImageSynthesisParam;
import com.alibaba.dashscope.exception.ApiException;
import com.alibaba.dashscope.exception.NoApiKeyException;
import com.alibaba.dashscope.task.AsyncTaskListParam;

/**
 * 通义万相绘画服务封装类
 * <p>
 * 重构说明：将底层实现委托给官方SDK的ImageSynthesis类，
 * 解决新版模型（如wanx2.1）因API路径更新导致的兼容性问题。
 */
public final class AdiImageSynthesis {
    // 委托对象：使用官方SDK的标准实现类，避免自定义封装底层API
    private final ImageSynthesis delegate = new ImageSynthesis();

    /**
     * 模型常量定义类
     * 包含通义万相所有支持的图片生成模型版本
     */
    public static class Models {
        /** 万相基础版模型v1 */
        public static final String WANX_V1 = "wanx-v1";
        /** 万相基础版模型v2 */
        public static final String WANX_V2 = "wanx-v2";
        /** 万相草图生图模型v1 */
        public static final String WANX_SKETCH_TO_IMAGE_V1 = "wanx-sketch-to-image-v1";
        /** 万相背景生成模型v2 */
        public static final String WANX_BACKGROUND_GENERATION_V2 = "wanx-background-generation-v2";
        /** 万相2.1版本高性能文生图模型（极速版） */
        public static final String WANX_2_1_T2I_TURBO = "wanx2.1-t2i-turbo";
    }

    /**
     * 默认构造方法
     * 使用官方SDK默认配置初始化
     */
    public AdiImageSynthesis() {
    }

    /**
     * 带任务类型和功能名称的构造方法
     * 保留该构造方法以兼容原有调用逻辑，内部无实际逻辑（底层由官方SDK接管）
     * @param task 图片合成任务类型（如text2image/image2image）
     * @param function 功能名称（如image-synthesis）
     */
    public AdiImageSynthesis(String task, String function) {
    }

    /**
     * 带任务类型、功能名称和自定义基础URL的构造方法
     * 保留该构造方法以兼容原有调用逻辑，内部无实际逻辑（底层由官方SDK接管）
     * @param task 图片合成任务类型（如text2image/image2image）
     * @param function 功能名称（如image-synthesis）
     * @param baseUrl 自定义API服务基础地址
     */
    public AdiImageSynthesis(String task, String function, String baseUrl) {
    }

    /**
     * 异步调用图片合成接口（通用参数）
     * @param param 图片合成参数（包含模型、提示词、分辨率等）
     * @return 图片合成结果（包含任务ID，需后续查询最终结果）
     * @throws NoApiKeyException 未配置API Key异常
     * @throws ApiException API调用失败异常（网络/参数/服务端错误）
     */
    public ImageSynthesisResult asyncCall(ImageSynthesisParam param)
            throws ApiException, NoApiKeyException {
        return delegate.asyncCall(param);
    }

    /**
     * 同步调用图片合成接口（通用参数）
     * 阻塞等待任务完成后返回结果
     * @param param 图片合成参数（包含模型、提示词、分辨率等）
     * @return 图片合成结果（包含生成的图片URL）
     * @throws NoApiKeyException 未配置API Key异常
     * @throws ApiException API调用失败异常（网络/参数/服务端错误）
     */
    public ImageSynthesisResult call(ImageSynthesisParam param)
            throws ApiException, NoApiKeyException {
        return delegate.call(param);
    }

    /**
     * 异步调用图片合成接口（草图生图专属参数）
     * @param param 草图生图参数（包含草图图片地址、提示词等）
     * @return 图片合成结果（包含任务ID，需后续查询最终结果）
     * @throws NoApiKeyException 未配置API Key异常
     * @throws ApiException API调用失败异常（网络/参数/服务端错误）
     */
    public ImageSynthesisResult asyncCall(SketchImageSynthesisParam param)
            throws ApiException, NoApiKeyException {
        return delegate.asyncCall(param);
    }

    /**
     * 同步调用图片合成接口（草图生图专属参数）
     * 阻塞等待任务完成后返回结果
     * @param param 草图生图参数（包含草图图片地址、提示词等）
     * @return 图片合成结果（包含生成的图片URL）
     * @throws NoApiKeyException 未配置API Key异常
     * @throws ApiException API调用失败异常（网络/参数/服务端错误）
     */
    public ImageSynthesisResult call(SketchImageSynthesisParam param)
            throws ApiException, NoApiKeyException {
        return delegate.call(param);
    }

    /**
     * 查询图片合成任务列表（传入封装参数）
     * @param param 任务列表查询参数（时间范围、模型、状态、分页等）
     * @return 任务列表结果
     * @throws NoApiKeyException 未配置API Key异常
     * @throws ApiException API调用失败异常（网络/参数/服务端错误）
     */
    public ImageSynthesisListResult list(AsyncTaskListParam param)
            throws ApiException, NoApiKeyException {
        return delegate.list(param);
    }

    /**
     * 查询图片合成任务列表（传入零散参数）
     * 自动封装为官方SDK的AsyncTaskListParam参数后调用
     * @param startTime 开始时间（格式：yyyy-MM-dd HH:mm:ss）
     * @param endTime 结束时间（格式：yyyy-MM-dd HH:mm:ss）
     * @param modelName 模型名称（如wanx2.1-t2i-turbo）
     * @param apiKeyId API Key ID
     * @param region 地域（如cn-beijing）
     * @param status 任务状态（如RUNNING/SUCCEEDED/FAILED）
     * @param pageNo 页码（从1开始）
     * @param pageSize 每页条数
     * @return 任务列表结果
     * @throws NoApiKeyException 未配置API Key异常
     * @throws ApiException API调用失败异常（网络/参数/服务端错误）
     */
    public ImageSynthesisListResult list(
            String startTime,
            String endTime,
            String modelName,
            String apiKeyId,
            String region,
            String status,
            Integer pageNo,
            Integer pageSize)
            throws ApiException, NoApiKeyException {

        AsyncTaskListParam param = AsyncTaskListParam.builder()
                .startTime(startTime)
                .endTime(endTime)
                .modelName(modelName)
                .apiKeyId(apiKeyId)
                .region(region)
                .status(status)
                .pageNo(pageNo)
                .pageSize(pageSize)
                .build();
        return delegate.list(param);
    }

    /**
     * 根据任务ID获取任务结果
     * @param taskId 任务唯一标识
     * @param apiKey API Key（可为null，内部会自动读取默认配置）
     * @return 图片合成任务结果
     * @throws NoApiKeyException 未配置API Key异常
     * @throws ApiException API调用失败异常（网络/参数/服务端错误）
     */
    public ImageSynthesisResult fetch(String taskId, String apiKey)
            throws ApiException, NoApiKeyException {
        return delegate.fetch(taskId, apiKey);
    }

    /**
     * 根据任务信息对象获取任务结果
     * 先校验任务信息有效性，再提取任务ID调用查询接口
     * @param taskInfo 任务信息对象（需包含有效的taskId）
     * @param apiKey API Key（可为null，内部会自动读取默认配置）
     * @return 图片合成任务结果
     * @throws NoApiKeyException 未配置API Key异常
     * @throws ApiException API调用失败异常（参数无效/网络/服务端错误）
     */
    public ImageSynthesisResult fetch(ImageSynthesisResult taskInfo, String apiKey)
            throws ApiException, NoApiKeyException {
        if (taskInfo == null || taskInfo.getOutput() == null) {
            throw new ApiException(new Exception("获取任务结果失败：传入的任务信息无效（空对象或无输出信息）"));
        }
        return delegate.fetch(taskInfo.getOutput().getTaskId(), apiKey);
    }

    /**
     * 根据任务ID取消任务
     * @param taskId 任务唯一标识
     * @param apiKey API Key（可为null，内部会自动读取默认配置）
     * @return 任务取消结果
     * @throws NoApiKeyException 未配置API Key异常
     * @throws ApiException API调用失败异常（网络/参数/服务端错误）
     */
    public ImageSynthesisResult cancel(String taskId, String apiKey)
            throws ApiException, NoApiKeyException {
        return delegate.cancel(taskId, apiKey);
    }

    /**
     * 根据任务信息对象取消任务
     * 先校验任务信息有效性，再提取任务ID调用取消接口
     * @param taskInfo 任务信息对象（需包含有效的taskId）
     * @param apiKey API Key（可为null，内部会自动读取默认配置）
     * @return 任务取消结果
     * @throws NoApiKeyException 未配置API Key异常
     * @throws ApiException API调用失败异常（参数无效/网络/服务端错误）
     */
    public ImageSynthesisResult cancel(ImageSynthesisResult taskInfo, String apiKey)
            throws ApiException, NoApiKeyException {
        if (taskInfo == null || taskInfo.getOutput() == null) {
            throw new ApiException(new Exception("取消任务失败：传入的任务信息无效（空对象或无输出信息）"));
        }
        return delegate.cancel(taskInfo.getOutput().getTaskId(), apiKey);
    }

    /**
     * 等待任务完成（映射为fetch方法）
     * 官方SDK未暴露wait方法，使用fetch方法轮询任务状态
     * @param taskId 任务唯一标识
     * @param apiKey API Key（可为null，内部会自动读取默认配置）
     * @return 图片合成任务结果
     * @throws NoApiKeyException 未配置API Key异常
     * @throws ApiException API调用失败异常（网络/参数/服务端错误）
     */
    public ImageSynthesisResult wait(String taskId, String apiKey)
            throws ApiException, NoApiKeyException {
        return delegate.fetch(taskId, apiKey);
    }

    /**
     * 等待任务完成（映射为fetch方法）
     * 先校验任务信息有效性，再提取任务ID轮询状态
     * @param taskInfo 任务信息对象（需包含有效的taskId）
     * @param apiKey API Key（可为null，内部会自动读取默认配置）
     * @return 图片合成任务结果
     * @throws NoApiKeyException 未配置API Key异常
     * @throws ApiException API调用失败异常（参数无效/网络/服务端错误）
     */
    public ImageSynthesisResult wait(ImageSynthesisResult taskInfo, String apiKey)
            throws ApiException, NoApiKeyException {
        if (taskInfo == null || taskInfo.getOutput() == null) {
            throw new ApiException(new Exception("等待任务完成失败：传入的任务信息无效（空对象或无输出信息）"));
        }
        return delegate.fetch(taskInfo.getOutput().getTaskId(), apiKey);
    }
}
