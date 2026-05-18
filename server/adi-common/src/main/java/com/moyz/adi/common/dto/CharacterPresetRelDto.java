package com.moyz.adi.common.dto;

import lombok.Data;

@Data
public class CharacterPresetRelDto {
    private Long id;
    private String uuid;
    private Long userCharacterId;
    private Long presetCharacterId;
}
