package com.moyz.adi.common.config;

import com.moyz.adi.common.enums.SegmentModeEnum;
import com.moyz.adi.common.interceptor.AdminAuthInterceptor;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.springframework.context.annotation.Configuration;
import org.springframework.format.FormatterRegistry;
import org.springframework.http.converter.HttpMessageConverters;
import org.springframework.http.converter.StringHttpMessageConverter;
import org.springframework.http.converter.json.JacksonJsonHttpMessageConverter;
import org.springframework.web.servlet.config.annotation.CorsRegistry;
import org.springframework.web.servlet.config.annotation.InterceptorRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;
import tools.jackson.databind.json.JsonMapper;

import java.nio.charset.StandardCharsets;

@Slf4j
@Configuration
public class WebMvcConfig implements WebMvcConfigurer {

    @Resource
    private JsonMapper jsonMapper;

    @Resource
    private AdminAuthInterceptor adminAuthInterceptor;

    @Override
    public void configureMessageConverters(HttpMessageConverters.ServerBuilder builder) {
        log.info("WebMvcConfig==configureMessageConverters");
        builder.withStringConverter(new StringHttpMessageConverter(StandardCharsets.UTF_8));
        builder.withJsonConverter(new JacksonJsonHttpMessageConverter(jsonMapper));
    }

    /**
     * Binds segmentMode query params / form fields by enum value, case-insensitively
     * (qa/text/parent_child)
     */
    @Override
    public void addFormatters(FormatterRegistry registry) {
        registry.addConverter(String.class, SegmentModeEnum.class, source -> {
            SegmentModeEnum mode = SegmentModeEnum.fromValue(source);
            if (mode == null) {
                throw new IllegalArgumentException("Invalid segmentMode: " + source);
            }
            return mode;
        });
    }

    @Override
    public void addCorsMappings(CorsRegistry registry) {
        registry.addMapping("/**")
                .allowedMethods("GET", "POST", "PUT", "DELETE")
                .allowedHeaders("*"); // 允许的请求头
    }

    @Override
    public void addInterceptors(InterceptorRegistry registry) {
        registry.addInterceptor(adminAuthInterceptor);
    }

}
