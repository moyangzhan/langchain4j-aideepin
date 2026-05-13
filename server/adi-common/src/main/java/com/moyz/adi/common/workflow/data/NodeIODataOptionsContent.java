package com.moyz.adi.common.workflow.data;

import com.moyz.adi.common.enums.WfIODataTypeEnum;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serial;
import java.io.Serializable;
import java.util.Map;

@EqualsAndHashCode(callSuper = true)
@Data
public class NodeIODataOptionsContent extends NodeIODataContent<Map<String, Object>> implements Serializable {

    @Serial
    private static final long serialVersionUID = 1L;

    private String title;

    private Integer type = WfIODataTypeEnum.OPTIONS.getValue();

    private Map<String, Object> value;
}