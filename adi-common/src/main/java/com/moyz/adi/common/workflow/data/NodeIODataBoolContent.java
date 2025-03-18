package com.moyz.adi.common.workflow.data;

import com.moyz.adi.common.enums.WfIODataTypeEnum;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serial;
import java.io.Serializable;

@EqualsAndHashCode(callSuper = true)
@Data
public class NodeIODataBoolContent extends NodeIODataContent<Boolean> implements Serializable {

    @Serial
    private static final long serialVersionUID = 1L;

    private String title;

    private Integer type = WfIODataTypeEnum.BOOL.getValue();

    private Boolean value;
}
