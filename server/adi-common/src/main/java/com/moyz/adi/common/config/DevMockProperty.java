package com.moyz.adi.common.config;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;

@Configuration
@ConfigurationProperties("adi.dev-mock")
@Data
public class DevMockProperty {
}
