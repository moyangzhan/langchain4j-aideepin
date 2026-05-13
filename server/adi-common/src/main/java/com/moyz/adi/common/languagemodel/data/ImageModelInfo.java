package com.moyz.adi.common.languagemodel.data;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.moyz.adi.common.languagemodel.AbstractImageModelService;
import lombok.Data;
import lombok.EqualsAndHashCode;

@EqualsAndHashCode(callSuper = true)
@Data
public class ImageModelInfo extends ModelInfo {

    @JsonIgnore
    private AbstractImageModelService modelService;
}
