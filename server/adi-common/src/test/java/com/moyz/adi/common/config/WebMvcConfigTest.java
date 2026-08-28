package com.moyz.adi.common.config;

import com.moyz.adi.common.enums.SegmentModeEnum;
import org.junit.jupiter.api.Test;
import org.springframework.core.convert.ConversionFailedException;
import org.springframework.format.support.DefaultFormattingConversionService;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

class WebMvcConfigTest {

    @Test
    void segmentModeConverterBindsByValueCaseInsensitively() {
        DefaultFormattingConversionService service = new DefaultFormattingConversionService();
        new WebMvcConfig().addFormatters(service);

        assertEquals(SegmentModeEnum.QA, service.convert("qa", SegmentModeEnum.class));
        assertEquals(SegmentModeEnum.TEXT, service.convert("text", SegmentModeEnum.class));
        assertEquals(SegmentModeEnum.PARENT_CHILD, service.convert("parent_child", SegmentModeEnum.class));
        assertEquals(SegmentModeEnum.TEXT, service.convert("TEXT", SegmentModeEnum.class));
    }

    @Test
    void segmentModeConverterRejectsUnknownValue() {
        DefaultFormattingConversionService service = new DefaultFormattingConversionService();
        new WebMvcConfig().addFormatters(service);

        assertThrows(ConversionFailedException.class, () -> service.convert("bogus", SegmentModeEnum.class));
    }
}
