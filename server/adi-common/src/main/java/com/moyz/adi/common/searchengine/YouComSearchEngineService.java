package com.moyz.adi.common.searchengine;

import com.fasterxml.jackson.databind.JsonNode;
import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.dto.SearchReturn;
import com.moyz.adi.common.dto.SearchReturnWebPage;
import com.moyz.adi.common.interfaces.AbstractSearchEngineService;
import com.moyz.adi.common.util.JsonUtil;
import com.moyz.adi.common.vo.YouComSetting;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.ResponseEntity;
import org.springframework.web.util.UriComponentsBuilder;

import java.net.InetSocketAddress;
import java.net.URI;
import java.util.ArrayList;
import java.util.List;

import static com.moyz.adi.common.cosntant.AdiConstant.SysConfigKey.YOUCOM_SETTING;

/**
 * You.com web search(https://documentation.you.com), search endpoint: GET https://ydc-index.io/v1/search
 * Auth: header X-API-Key
 */
@Slf4j
public class YouComSearchEngineService extends AbstractSearchEngineService<YouComSetting> {

    private static final int MAX_COUNT = 20;
    private static final int DEFAULT_COUNT = 10;

    public YouComSearchEngineService(InetSocketAddress proxy) {
        super(AdiConstant.SearchEngineName.YOUCOM, YOUCOM_SETTING, YouComSetting.class, proxy);
    }

    @Override
    protected void initSearchEngine() {
        //No sdk/client to build, the http call is issued directly inside search()
    }

    @Override
    public boolean isEnabled() {
        return null != setting && StringUtils.isNotBlank(setting.getKey());
    }

    @Override
    public SearchReturn search(String searchTxt, String country, String language, Integer topN) {
        SearchReturn result = new SearchReturn();
        List<SearchReturnWebPage> items = new ArrayList<>();
        result.setItems(items);
        if (StringUtils.isBlank(searchTxt)) {
            log.warn("youcom search terms is empty");
            return result;
        }
        try {
            int count = null == topN || topN <= 0 ? DEFAULT_COUNT : Math.min(topN, MAX_COUNT);
            URI uri = UriComponentsBuilder.fromHttpUrl(setting.getUrl())
                    .queryParam("query", searchTxt)
                    .queryParam("count", count)
                    .encode()
                    .build()
                    .toUri();

            HttpHeaders headers = new HttpHeaders();
            headers.set("X-API-Key", setting.getKey());
            HttpEntity<Void> entity = new HttpEntity<>(headers);

            ResponseEntity<String> response = getRestTemplate().exchange(uri, HttpMethod.GET, entity, String.class);
            JsonNode root = JsonUtil.getObjectMapper().readTree(response.getBody());
            JsonNode webResults = root.path("results").path("web");
            if (webResults.isArray() && !webResults.isEmpty()) {
                for (JsonNode item : webResults) {
                    items.add(toWebPage(item));
                }
            } else {
                log.warn("youcom search result is empty");
            }
        } catch (Exception e) {
            log.error("youcom search error", e);
        }
        return result;
    }

    private SearchReturnWebPage toWebPage(JsonNode item) {
        String title = item.path("title").asText("");
        String link = item.path("url").asText("");
        String snippet = item.path("description").asText("");
        StringBuilder content = new StringBuilder();
        JsonNode snippets = item.path("snippets");
        if (snippets.isArray()) {
            for (JsonNode s : snippets) {
                if (!content.isEmpty()) {
                    content.append("\n");
                }
                content.append(s.asText(""));
            }
        }
        return SearchReturnWebPage.builder()
                .title(title)
                .link(link)
                .snippet(snippet)
                .content(content.toString())
                .build();
    }

}
