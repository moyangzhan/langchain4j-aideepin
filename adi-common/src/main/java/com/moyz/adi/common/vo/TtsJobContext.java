package com.moyz.adi.common.vo;

import com.moyz.adi.common.helper.TtsModelContext;
import lombok.Data;

/**
 * TTS任务中的各种临时数据（非策略模式概念中的Context）
 */
@Data
public class TtsJobContext {
    private String jobId;
    private TtsModelContext ttsModelContext;
}
