package com.moyz.adi.common.dto;

import lombok.AllArgsConstructor;
import lombok.Data;

import java.util.List;

@Data
@AllArgsConstructor
public class CharacterMsgListResp {

    private String minMsgUuid;

    private List<CharacterMsgDto> msgList;
}
