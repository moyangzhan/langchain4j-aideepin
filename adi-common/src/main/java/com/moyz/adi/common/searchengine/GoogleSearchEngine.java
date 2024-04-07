package com.moyz.adi.common.searchengine;

import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.dto.GoogleSearchResp;
import com.moyz.adi.common.dto.SearchResult;
import com.moyz.adi.common.dto.SearchResultItem;
import com.moyz.adi.common.interfaces.AbstractSearchEngine;
import com.moyz.adi.common.util.MPPageUtil;
import com.moyz.adi.common.vo.GoogleSetting;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.List;

import static com.moyz.adi.common.cosntant.AdiConstant.SysConfigKey.GOOGLE_SETTING;

@Slf4j
public class GoogleSearchEngine extends AbstractSearchEngine<GoogleSetting> {

    public GoogleSearchEngine() {
        super(AdiConstant.SearchEngineName.GOOGLE, GOOGLE_SETTING, GoogleSetting.class);
    }

    @Override
    public boolean isEnabled() {
        return StringUtils.isNoneBlank(setting.getKey(), setting.getCx());
    }

    @Override
    public SearchResult search(String searchTxt) {
        SearchResult result = new SearchResult();
        List<SearchResultItem> items = new ArrayList<>();
        try {
            ResponseEntity<GoogleSearchResp> resp = getRestTemplate().getForEntity(MessageFormat.format("{0}?key={1}&cx={2}&q={3}", setting.getUrl(), setting.getKey(), setting.getCx(), searchTxt), GoogleSearchResp.class);
            if (null != resp && HttpStatus.OK.isSameCodeAs(resp.getStatusCode())) {
                GoogleSearchResp googleSearchResp = resp.getBody();
                if (null != googleSearchResp.getError()) {
                    log.error("google search error,code:{},message:{}", googleSearchResp.getError().getCode(), googleSearchResp.getError().getMessage());
                    result.setErrorMessage(googleSearchResp.getError().getMessage());
                } else {
                    log.info("google response:{}", resp);
                    items = MPPageUtil.convertTo(googleSearchResp.getItems(), SearchResultItem.class);
                }
            }
        } catch (Exception e) {
            log.error("google search error", e);
        }
        result.setItems(items);
        return result;
    }

}
