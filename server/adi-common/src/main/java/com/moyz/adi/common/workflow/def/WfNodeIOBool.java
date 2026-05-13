package com.moyz.adi.common.workflow.def;

import com.moyz.adi.common.enums.WfIODataTypeEnum;
import com.moyz.adi.common.workflow.data.NodeIOData;
import com.moyz.adi.common.workflow.data.NodeIODataBoolContent;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;

/**
 * 用户输入参数-布尔类型 参数定义
 */
@EqualsAndHashCode(callSuper = true)
@Data
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
public class WfNodeIOBool extends WfNodeIO {

    protected Integer type = WfIODataTypeEnum.BOOL.getValue();

    @Override
    public boolean checkValue(NodeIOData data) {
        if (!(data.getContent() instanceof NodeIODataBoolContent)) {
            return false;
        }
        return !required || null != data.getContent().getValue();
    }
}
