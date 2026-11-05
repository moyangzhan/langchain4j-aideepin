package com.moyz.adi.common.config;

import com.moyz.adi.common.enums.SegmentModeEnum;
import com.moyz.adi.common.interceptor.AdminAuthInterceptor;
import org.junit.jupiter.api.Test;
import org.springframework.core.convert.ConversionFailedException;
import org.springframework.format.support.DefaultFormattingConversionService;
import org.springframework.http.converter.ByteArrayHttpMessageConverter;
import org.springframework.http.converter.HttpMessageConverter;
import org.springframework.http.converter.HttpMessageConverters;
import org.springframework.http.converter.ResourceHttpMessageConverter;
import org.springframework.http.converter.StringHttpMessageConverter;
import org.springframework.http.converter.json.JacksonJsonHttpMessageConverter;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.web.servlet.config.annotation.InterceptorRegistration;
import org.springframework.web.servlet.config.annotation.InterceptorRegistry;
import tools.jackson.databind.json.JsonMapper;

import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

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

    /**
     * converter 配置契约：String 槽位必须是 UTF-8（SSE 字符串数据经此序列化，ISO-8859-1 会把
     * 非 Latin-1 字符写成 "?"）、JSON 槽位是项目的 JsonMapper、默认集（ByteArray/Resource 等）
     * 保持注册不被整体替换。按 WebMvcConfigurationSupport 的真实调用序组装 builder。
     */
    @Test
    void converterSlotsPinUtf8StringProjectJsonMapperAndDefaultSet() {
        WebMvcConfig config = new WebMvcConfig();
        ReflectionTestUtils.setField(config, "jsonMapper", JsonMapper.builder().build());

        HttpMessageConverters.ServerBuilder builder = HttpMessageConverters.forServer().registerDefaults();
        config.configureMessageConverters(builder);

        List<HttpMessageConverter<?>> converters = new ArrayList<>();
        builder.build().forEach(converters::add);

        List<HttpMessageConverter<?>> stringSlots = converters.stream()
                .filter(StringHttpMessageConverter.class::isInstance)
                .toList();
        assertEquals(1, stringSlots.size());
        StringHttpMessageConverter stringConverter = (StringHttpMessageConverter) stringSlots.getFirst();
        assertEquals(StandardCharsets.UTF_8, stringConverter.getDefaultCharset());

        assertTrue(converters.stream().anyMatch(JacksonJsonHttpMessageConverter.class::isInstance));
        assertTrue(converters.stream().anyMatch(ByteArrayHttpMessageConverter.class::isInstance));
        assertTrue(converters.stream().anyMatch(ResourceHttpMessageConverter.class::isInstance));
    }

    /**
     * 拦截器注册契约：AdminAuthInterceptor 必须注册，且不配任何 path pattern——
     * 包判定是唯一依据，加 pattern 会把覆盖面重新绑回 URL。
     */
    @Test
    void registersAdminAuthInterceptorWithoutPathPatterns() {
        WebMvcConfig config = new WebMvcConfig();
        AdminAuthInterceptor interceptor = new AdminAuthInterceptor();
        ReflectionTestUtils.setField(config, "adminAuthInterceptor", interceptor);
        InterceptorRegistry registry = mock(InterceptorRegistry.class);
        InterceptorRegistration registration = mock(InterceptorRegistration.class);
        when(registry.addInterceptor(any())).thenReturn(registration);

        config.addInterceptors(registry);

        verify(registry).addInterceptor(interceptor);
        verifyNoInteractions(registration);
    }
}
