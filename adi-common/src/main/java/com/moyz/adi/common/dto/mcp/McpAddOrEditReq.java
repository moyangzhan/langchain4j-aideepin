package com.moyz.adi.common.dto.mcp;

import com.moyz.adi.common.annotation.ElementInArray;
import jakarta.validation.constraints.NotBlank;
import lombok.Data;
import org.springframework.validation.annotation.Validated;

import java.util.List;

import static com.moyz.adi.common.cosntant.AdiConstant.McpConstant.*;

@Data
@Validated
public class McpAddOrEditReq {

    private String uuid;

    @NotBlank
    private String title;

    @ElementInArray(acceptedValues = {
            TRANSPORT_TYPE_SSE,
            TRANSPORT_TYPE_STDIO
    }, required = true)
    private String transportType;

    private String sseUrl;

    private Integer sseTimeout;

    private String stdioCommand;

    private String stdioArg;

    private List<McpCommonParam> presetParams;

    private List<McpCustomizedParamDefinition> customizedParamDefinitions;

    @ElementInArray(acceptedValues = {
            INSTALL_TYPE_DOCKER,
            INSTALL_TYPE_LOCAL,
            INSTALL_TYPE_REMOTE,
            INSTALL_TYPE_WASM
    }, required = true)
    private String installType;

    private String repoUrl;

    private String remark;

    private Boolean isEnable;
}
