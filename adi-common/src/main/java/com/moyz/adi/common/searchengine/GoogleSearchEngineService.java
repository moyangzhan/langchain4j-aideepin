package com.moyz.adi.common.searchengine;

import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.dto.SearchReturn;
import com.moyz.adi.common.dto.SearchReturnWebPage;
import com.moyz.adi.common.interfaces.AbstractSearchEngineService;
import com.moyz.adi.common.vo.GoogleSetting;
import dev.langchain4j.web.search.WebSearchResults;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;

import java.net.Proxy;
import java.util.ArrayList;
import java.util.List;

import static com.moyz.adi.common.cosntant.AdiConstant.SysConfigKey.GOOGLE_SETTING;

@Slf4j
public class GoogleSearchEngineService extends AbstractSearchEngineService<GoogleSetting> {

    public GoogleSearchEngineService(Proxy proxy) {
        super(AdiConstant.SearchEngineName.GOOGLE, GOOGLE_SETTING, GoogleSetting.class, proxy);
    }

    @Override
    protected void initSearchEngine() {
        searchEngine = AdiGoogleCustomWebSearchEngine.builder()
                .apiKey(setting.getKey())
                .csi(setting.getCx())
                .siteRestrict(false)
                .includeImages(false)
                .logRequests(true)
                .logResponses(true)
                .proxy(proxy)
                .build();
    }

    @Override
    public boolean isEnabled() {
        return StringUtils.isNoneBlank(setting.getKey(), setting.getCx());
    }

    @Override
    public SearchReturn search(String searchTxt) {
        SearchReturn result = new SearchReturn();
        List<SearchReturnWebPage> items = new ArrayList<>();
        try {
            WebSearchResults webSearchResults = searchEngine.search(searchTxt);
            if (null != webSearchResults && webSearchResults.searchInformation().totalResults() > 0) {
                List<SearchReturnWebPage> wrapItems = webSearchResults.results()
                        .stream()
                        .map(
                                item -> SearchReturnWebPage.builder()
                                        .title(item.title())
                                        .link(item.url().toString())
                                        .snippet(item.snippet())
                                        .content(item.content())
                                        .build()
                        )
                        .toList();
                items.addAll(wrapItems);
            } else {
                log.warn("google search result is empty");
            }
        } catch (Exception e) {
            log.error("google search error", e);
        }
        result.setItems(items);
        return result;
    }

}
