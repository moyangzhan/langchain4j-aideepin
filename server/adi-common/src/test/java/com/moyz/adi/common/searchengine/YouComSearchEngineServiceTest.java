package com.moyz.adi.common.searchengine;

import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.dto.SearchReturn;
import com.moyz.adi.common.dto.SearchReturnWebPage;
import com.moyz.adi.common.util.LocalCache;
import com.moyz.adi.common.util.SpringUtil;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.EnabledIfEnvironmentVariable;
import org.mockito.Mockito;
import org.springframework.context.ApplicationContext;
import org.springframework.http.HttpMethod;
import org.springframework.http.MediaType;
import org.springframework.test.web.client.MockRestServiceServer;
import org.springframework.web.client.RestTemplate;

import static org.assertj.core.api.Assertions.assertThat;
import static org.springframework.test.web.client.match.MockRestRequestMatchers.header;
import static org.springframework.test.web.client.match.MockRestRequestMatchers.method;
import static org.springframework.test.web.client.match.MockRestRequestMatchers.queryParam;
import static org.springframework.test.web.client.match.MockRestRequestMatchers.requestTo;
import static org.springframework.test.web.client.response.MockRestResponseCreators.withServerError;
import static org.springframework.test.web.client.response.MockRestResponseCreators.withSuccess;

/**
 * Offline unit tests for {@link YouComSearchEngineService}. All HTTP calls are stubbed via
 * {@link MockRestServiceServer}; nothing here hits the real https://ydc-index.io endpoint.
 * See {@link #liveSearch_realApi_returnsResults()} for the one gated live call.
 */
class YouComSearchEngineServiceTest {

    private static final String URL = "https://ydc-index.io/v1/search";

    private RestTemplate restTemplate;
    private MockRestServiceServer mockServer;

    @BeforeEach
    void setUp() {
        restTemplate = new RestTemplate();
        mockServer = MockRestServiceServer.createServer(restTemplate);

        ApplicationContext ctx = Mockito.mock(ApplicationContext.class);
        Mockito.when(ctx.getBean(RestTemplate.class)).thenReturn(restTemplate);
        new SpringUtil().setApplicationContext(ctx);

        LocalCache.CONFIGS.clear();
    }

    @AfterEach
    void tearDown() {
        LocalCache.CONFIGS.remove(AdiConstant.SysConfigKey.YOUCOM_SETTING);
    }

    private YouComSearchEngineService newService(String key) {
        String json = "{\"url\":\"" + URL + "\",\"key\":\"" + key + "\"}";
        LocalCache.CONFIGS.put(AdiConstant.SysConfigKey.YOUCOM_SETTING, json);
        return new YouComSearchEngineService(null);
    }

    @Test
    void isEnabled_whenKeyPresent_returnsTrue() {
        assertThat(newService("test-key").isEnabled()).isTrue();
    }

    @Test
    void isEnabled_whenKeyBlank_returnsFalse() {
        assertThat(newService("").isEnabled()).isFalse();
    }

    @Test
    void search_sendsApiKeyHeaderAndQueryParams() {
        YouComSearchEngineService service = newService("secret-key");
        mockServer.expect(requestTo(startsWithMatcher(URL)))
                .andExpect(method(HttpMethod.GET))
                .andExpect(header("X-API-Key", "secret-key"))
                .andExpect(queryParam("query", "hello%20world"))
                .andExpect(queryParam("count", "7"))
                .andRespond(withSuccess("{\"results\":{\"web\":[]}}", MediaType.APPLICATION_JSON));

        SearchReturn result = service.search("hello world", null, null, 7);

        assertThat(result.getItems()).isEmpty();
        mockServer.verify();
    }

    @Test
    void search_capsCountAt20() {
        YouComSearchEngineService service = newService("secret-key");
        mockServer.expect(requestTo(startsWithMatcher(URL)))
                .andExpect(queryParam("count", "20"))
                .andRespond(withSuccess("{\"results\":{\"web\":[]}}", MediaType.APPLICATION_JSON));

        service.search("anything", null, null, 100);

        mockServer.verify();
    }

    @Test
    void search_defaultsCountWhenTopNMissing() {
        YouComSearchEngineService service = newService("secret-key");
        mockServer.expect(requestTo(startsWithMatcher(URL)))
                .andExpect(queryParam("count", "10"))
                .andRespond(withSuccess("{\"results\":{\"web\":[]}}", MediaType.APPLICATION_JSON));

        service.search("anything", null, null, null);

        mockServer.verify();
    }

    @Test
    void search_mapsWebResultsIncludingMissingOptionalFields() {
        YouComSearchEngineService service = newService("secret-key");
        String body = "{\"results\":{\"web\":["
                + "{\"title\":\"Title A\",\"url\":\"https://a.example\",\"description\":\"Desc A\",\"snippets\":[\"s1\",\"s2\"]},"
                + "{\"title\":\"Title B\",\"url\":\"https://b.example\"}"
                + "]}}";
        mockServer.expect(requestTo(startsWithMatcher(URL)))
                .andRespond(withSuccess(body, MediaType.APPLICATION_JSON));

        SearchReturn result = service.search("hello", null, null, 5);

        assertThat(result.getItems()).hasSize(2);
        SearchReturnWebPage first = result.getItems().get(0);
        assertThat(first.getTitle()).isEqualTo("Title A");
        assertThat(first.getLink()).isEqualTo("https://a.example");
        assertThat(first.getSnippet()).isEqualTo("Desc A");
        assertThat(first.getContent()).isEqualTo("s1\ns2");

        SearchReturnWebPage second = result.getItems().get(1);
        assertThat(second.getTitle()).isEqualTo("Title B");
        assertThat(second.getLink()).isEqualTo("https://b.example");
        assertThat(second.getSnippet()).isEmpty();
        assertThat(second.getContent()).isEmpty();
    }

    @Test
    void search_blankQuery_returnsEmptyWithoutCallingApi() {
        YouComSearchEngineService service = newService("secret-key");

        SearchReturn result = service.search("   ", null, null, 5);

        assertThat(result.getItems()).isEmpty();
        mockServer.verify();
    }

    @Test
    void search_serverError_returnsEmptyResultNeverThrows() {
        YouComSearchEngineService service = newService("secret-key");
        mockServer.expect(requestTo(startsWithMatcher(URL)))
                .andRespond(withServerError());

        SearchReturn result = service.search("hello", null, null, 5);

        assertThat(result.getItems()).isEmpty();
    }

    @Test
    void search_malformedJson_returnsEmptyResultNeverThrows() {
        YouComSearchEngineService service = newService("secret-key");
        mockServer.expect(requestTo(startsWithMatcher(URL)))
                .andRespond(withSuccess("not-json", MediaType.APPLICATION_JSON));

        SearchReturn result = service.search("hello", null, null, 5);

        assertThat(result.getItems()).isEmpty();
    }

    private static org.hamcrest.Matcher<String> startsWithMatcher(String prefix) {
        return org.hamcrest.Matchers.startsWith(prefix);
    }

    /**
     * One real, network-hitting call. Skipped entirely unless YDC_API_KEY is exported
     * (see ~/Desktop/you.com_projects/.env). Never runs as part of the default offline suite.
     */
    @Test
    @EnabledIfEnvironmentVariable(named = "YDC_API_KEY", matches = ".+")
    void liveSearch_realApi_returnsResults() {
        String key = System.getenv("YDC_API_KEY");
        RestTemplate realRestTemplate = new RestTemplate();
        ApplicationContext ctx = Mockito.mock(ApplicationContext.class);
        Mockito.when(ctx.getBean(RestTemplate.class)).thenReturn(realRestTemplate);
        new SpringUtil().setApplicationContext(ctx);

        String json = "{\"url\":\"" + URL + "\",\"key\":\"" + key + "\"}";
        LocalCache.CONFIGS.put(AdiConstant.SysConfigKey.YOUCOM_SETTING, json);
        YouComSearchEngineService service = new YouComSearchEngineService(null);

        SearchReturn result = service.search("You.com API", null, null, 3);

        assertThat(result.getErrorMessage()).isNull();
        assertThat(result.getItems()).isNotEmpty();
    }
}
