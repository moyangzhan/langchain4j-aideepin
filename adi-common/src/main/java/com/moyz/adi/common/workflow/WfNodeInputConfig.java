package com.moyz.adi.common.workflow;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.moyz.adi.common.workflow.def.WfNodeParamRef;
import com.moyz.adi.common.workflow.def.WfNodeIO;
import jakarta.validation.constraints.NotNull;
import lombok.Data;
import org.springframework.validation.annotation.Validated;

import java.util.List;

/**
 * 节点的输入参数配置
 */
@Validated
@Data
public class WfNodeInputConfig {

    @NotNull
    @JsonProperty("user_inputs")
    private List<WfNodeIO> userInputs;

    @NotNull
    @JsonProperty("ref_inputs")
    private List<WfNodeParamRef> refInputs;
}
