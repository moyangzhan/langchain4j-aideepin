package com.moyz.adi.common.dto;

import lombok.Data;

import java.util.List;

@Data
public class AiImagesListResp {
    private Long minId;
    private List<AiImageDto> imageItems;
}
