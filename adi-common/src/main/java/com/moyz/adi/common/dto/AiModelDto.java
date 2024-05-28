package com.moyz.adi.common.dto;

import lombok.Data;

import java.time.LocalDateTime;

@Data
public class AiModelDto {

    private Long id;

    private String type;

    private String name;

    private String platform;

    private String remark;

    private Boolean isEnable;

    private LocalDateTime createTime;

    private LocalDateTime updateTime;

}
