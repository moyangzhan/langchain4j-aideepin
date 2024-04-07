package com.moyz.adi.common.searchengine;

import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.interfaces.AbstractSearchEngine;
import com.moyz.adi.common.vo.SearchEngineInfo;
import lombok.extern.slf4j.Slf4j;

import java.util.LinkedHashMap;
import java.util.Map;

/**
 * Search engine context. strategy design model
 */
@Slf4j
public class SearchEngineContext {
    public static final Map<String, SearchEngineInfo> NAME_TO_ENGINE = new LinkedHashMap<>();

    private AbstractSearchEngine iSearchEngine;

    public SearchEngineContext(String searchEngineName) {
        if (null == NAME_TO_ENGINE.get(searchEngineName)) {
            log.warn("︿︿︿ Can not find {}, use the default engine GOOGLE ︿︿︿", searchEngineName);
            iSearchEngine = NAME_TO_ENGINE.get(AdiConstant.SearchEngineName.GOOGLE).getEngine();
        } else {
            iSearchEngine = NAME_TO_ENGINE.get(searchEngineName).getEngine();
        }
    }

    public static void addEngine(String engineName, AbstractSearchEngine searchEngine) {
        SearchEngineInfo info = new SearchEngineInfo();
        info.setName(engineName);
        info.setEnable(searchEngine.isEnabled());
        info.setEngine(searchEngine);
        NAME_TO_ENGINE.put(engineName, info);
    }

    public AbstractSearchEngine getEngine() {
        return iSearchEngine;
    }
}
