package com.moyz.adi.common.workflow.def;

import com.moyz.adi.common.enums.WfIODataTypeEnum;
import com.moyz.adi.common.workflow.data.NodeIOData;
import com.moyz.adi.common.workflow.data.NodeIODataFilesContent;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;
import org.apache.commons.collections4.CollectionUtils;

/**
 * 用户输入参数-文件列表类型 参数定义
 */
@EqualsAndHashCode(callSuper = true)
@Data
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
public class WfNodeIOFiles extends WfNodeIO {
    protected Integer type = WfIODataTypeEnum.FILES.getValue();
    private Integer limit;

    @Override
    public boolean checkValue(NodeIOData data) {
        if (!(data.getContent() instanceof NodeIODataFilesContent wfNodeIOFiles)) {
            return false;
        }
        return !required || !CollectionUtils.isEmpty(wfNodeIOFiles.getValue());
    }
}
