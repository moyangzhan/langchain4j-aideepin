package com.moyz.adi.common.interfaces;

import com.moyz.adi.common.dto.SearchReturn;
import com.moyz.adi.common.util.JsonUtil;
import com.moyz.adi.common.util.LocalCache;
import com.moyz.adi.common.util.SpringUtil;
import dev.langchain4j.web.search.WebSearchEngine;
import org.springframework.http.client.SimpleClientHttpRequestFactory;
import org.springframework.web.client.RestTemplate;

import java.net.Proxy;

public abstract class AbstractSearchEngineService<T> {

    protected String engineName;

    protected Proxy proxy;

    protected WebSearchEngine searchEngine;

    protected AbstractSearchEngineService(String engineName, String settingName, Class<T> clazz, Proxy proxy) {
        this.engineName = engineName;
        String st = LocalCache.CONFIGS.get(settingName);
        setting = JsonUtil.fromJson(st, clazz);
        this.proxy = proxy;
        initSearchEngine();
    }

    protected abstract void initSearchEngine();

    protected T setting;

    public abstract boolean isEnabled();

    public String getEngineName() {
        return engineName;
    }

    public abstract SearchReturn search(String searchTxt);

    protected RestTemplate getRestTemplate() {
        RestTemplate restTemplate = SpringUtil.getBean(RestTemplate.class);
        if (null != proxy) {
            SimpleClientHttpRequestFactory requestFactory = new SimpleClientHttpRequestFactory();
            requestFactory.setProxy(proxy);
            restTemplate.setRequestFactory(requestFactory);
        }
        return restTemplate;
    }
}
