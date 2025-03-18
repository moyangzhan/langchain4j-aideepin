package com.moyz.adi.common.workflow.def;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.moyz.adi.common.enums.WfIODataTypeEnum;
import com.moyz.adi.common.workflow.data.NodeIOData;
import com.moyz.adi.common.workflow.data.NodeIODataTextContent;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;

/**
 * 用户输入参数-文本类型 参数定义
 */
@EqualsAndHashCode(callSuper = true)
@Data
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
public class WfNodeIOText extends WfNodeIO {

    protected Integer type = WfIODataTypeEnum.TEXT.getValue();

    @JsonProperty("max_length")
    private Integer maxLength;

    @Override
    public boolean checkValue(NodeIOData data) {
        if (!(data.getContent() instanceof NodeIODataTextContent optionsData)) {
            return false;
        }
        String value = optionsData.getValue();
        if (required && null == value) {
            return false;
        }
        if (null != maxLength && value.length() > maxLength) {
            return false;
        }
        return true;
    }
}
