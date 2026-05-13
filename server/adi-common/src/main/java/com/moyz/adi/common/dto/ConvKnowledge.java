package com.moyz.adi.common.dto;

import lombok.Data;

/**
 * 会话中使用的知识库信息<br/>
 * 如果是其他人的知识库，并且该知识库已经由公开转为私有，则返回的{kbInfo}为null,isEnable为false
 */
@Data
public class ConvKnowledge {
    private Long id;
    private String uuid;
    private String title;
    private Boolean isMine;
    private Boolean isPublic;
    private KbInfoResp kbInfo;
    private Boolean isEnable;
}
