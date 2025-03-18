package com.moyz.adi.common.workflow.data;

import com.moyz.adi.common.enums.WfIODataTypeEnum;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serial;
import java.io.Serializable;
import java.util.List;

@EqualsAndHashCode(callSuper = true)
@Data
public class NodeIODataFilesContent extends NodeIODataContent<List<String>> implements Serializable {

    @Serial
    private static final long serialVersionUID = 1L;

    private String title;

    private Integer type = WfIODataTypeEnum.FILES.getValue();

    private List<String> value;
}