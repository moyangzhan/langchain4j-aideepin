package com.moyz.adi.common.service;

import ch.qos.logback.classic.Logger;
import ch.qos.logback.classic.Level;
import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.classic.spi.ThrowableProxyUtil;
import ch.qos.logback.core.read.ListAppender;
import com.baomidou.mybatisplus.core.MybatisConfiguration;
import com.baomidou.mybatisplus.core.conditions.AbstractWrapper;
import com.baomidou.mybatisplus.core.metadata.TableInfoHelper;
import com.baomidou.mybatisplus.core.override.MybatisMapperProxy;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.moyz.adi.common.base.ThreadContext;
import com.moyz.adi.common.dto.UserMcpDto;
import com.moyz.adi.common.dto.mcp.UserMcpUpdateReq;
import com.moyz.adi.common.entity.AiModel;
import com.moyz.adi.common.entity.Character;
import com.moyz.adi.common.entity.Mcp;
import com.moyz.adi.common.entity.User;
import com.moyz.adi.common.entity.UserMcp;
import com.moyz.adi.common.languagemodel.AbstractLLMService;
import com.moyz.adi.common.helper.SseManager;
import com.moyz.adi.common.mapper.McpMapper;
import com.moyz.adi.common.mapper.UserMcpMapper;
import com.moyz.adi.common.util.AesUtil;
import com.moyz.adi.common.util.CharacterChatHelper;
import com.moyz.adi.common.util.SpringUtil;
import com.moyz.adi.common.vo.ChatModelRequest;
import com.moyz.adi.common.vo.SseAskParam;
import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpServer;
import dev.langchain4j.agent.tool.ToolExecutionRequest;
import dev.langchain4j.data.message.AiMessage;
import dev.langchain4j.mcp.client.McpClient;
import dev.langchain4j.mcp.client.DefaultMcpClient;
import dev.langchain4j.model.chat.ChatModel;
import dev.langchain4j.model.chat.request.ChatRequest;
import dev.langchain4j.model.chat.response.ChatResponse;
import org.apache.ibatis.builder.MapperBuilderAssistant;
import org.apache.ibatis.session.SqlSession;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.slf4j.LoggerFactory;
import org.springframework.context.support.GenericApplicationContext;
import org.springframework.test.util.ReflectionTestUtils;

import java.io.IOException;
import java.io.OutputStream;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

/** Real SDK/host path; database, model and MCP service are synthetic, without external network or keys. */
class UserMcpHttpBindingTest {
    private final ObjectMapper json = new ObjectMapper().configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
    private final Map<Long, Mcp> services = new ConcurrentHashMap<>();
    private final Map<String, UserMcp> stored = new ConcurrentHashMap<>();
    private final List<Request> requests = new CopyOnWriteArrayList<>();
    private final List<McpClient> clients = new ArrayList<>();
    private final AtomicInteger calls = new AtomicInteger();
    private final AtomicInteger deletes = new AtomicInteger();
    private UserMcpService service;
    private UserMcpMapper mapper;
    private HttpServer http;
    private GenericApplicationContext context;
    private ListAppender<ILoggingEvent> logs;
    private String oldAesKey;
    private OutputStream sseStream;

    @BeforeEach
    void setup() throws Exception {
        oldAesKey = AesUtil.AES_KEY;
        AesUtil.AES_KEY = "0123456789abcdef";
        TableInfoHelper.initTableInfo(new MapperBuilderAssistant(new MybatisConfiguration(), "fixture"), UserMcp.class);
        TableInfoHelper.initTableInfo(new MapperBuilderAssistant(new MybatisConfiguration(), "fixture"), Mcp.class);
        mapper = fixtureMapper(UserMcpMapper.class);
        when(mapper.selectOne(any())).thenAnswer(i -> copy(stored.get(key(ThreadContext.getCurrentUserId(), 100L)), UserMcp.class));
        when(mapper.selectList(any())).thenAnswer(i -> {
            AbstractWrapper<?, ?, ?> wrapper = i.getArgument(0);
            String sql = wrapper.getSqlSegment();
            assertTrue(sql.contains("user_id"));
            Map<String, Object> params = wrapper.getParamNameValuePairs();
            return stored.values().stream().filter(u -> params.containsValue(u.getUserId()) && params.containsValue(u.getMcpId())
                    && Boolean.TRUE.equals(u.getIsEnable()) && !Boolean.TRUE.equals(u.getIsDeleted()))
                    .map(u -> copy(u, UserMcp.class)).toList();
        });
        when(mapper.insert(any(UserMcp.class))).thenAnswer(i -> {
            UserMcp u = i.getArgument(0);
            u.setId(u.getMcpId() + u.getUserId());
            u.setIsDeleted(false);
            stored.put(key(u.getUserId(), u.getMcpId()), copy(u, UserMcp.class));
            return 1;
        });
        when(mapper.updateById(any(UserMcp.class))).thenAnswer(i -> {
            UserMcp update = i.getArgument(0);
            UserMcp old = stored.values().stream().filter(u -> u.getId().equals(update.getId())).findFirst().orElseThrow();
            UserMcp merged = copy(old, UserMcp.class);
            if (update.getMcpCustomizedParams() != null) merged.setMcpCustomizedParams(update.getMcpCustomizedParams());
            if (update.getIsEnable() != null) merged.setIsEnable(update.getIsEnable());
            stored.put(key(old.getUserId(), old.getMcpId()), copy(merged, UserMcp.class));
            return 1;
        });
        McpMapper mcpMapper = fixtureMapper(McpMapper.class);
        when(mcpMapper.selectList(any())).thenAnswer(i -> {
            AbstractWrapper<?, ?, ?> wrapper = i.getArgument(0);
            wrapper.getSqlSegment();
            return services.values().stream().filter(m -> wrapper.getParamNameValuePairs().containsValue(m.getId()))
                    .map(m -> copy(m, Mcp.class)).toList();
        });
        when(mcpMapper.selectOne(any(), anyBoolean())).thenAnswer(i -> copy(services.get(100L), Mcp.class));
        when(mcpMapper.selectOne(any())).thenAnswer(i -> copy(services.get(100L), Mcp.class));
        McpService mcpService = new McpService();
        ReflectionTestUtils.setField(mcpService, "baseMapper", mcpMapper);
        service = new UserMcpService();
        ReflectionTestUtils.setField(service, "baseMapper", mapper);
        ReflectionTestUtils.setField(service, "mcpService", mcpService);
        context = new GenericApplicationContext();
        context.registerBean(UserMcpService.class, () -> service);
        context.registerBean(McpService.class, () -> mcpService);
        context.registerBean(SseManager.class, SseManager::new);
        context.refresh();
        new SpringUtil().setApplicationContext(context);
        http = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
        http.createContext("/mcp", this::handle);
        http.createContext("/sse", this::handleSse);
        http.createContext("/messages", this::handleSseMessage);
        http.start();
        logs = new ListAppender<>();
        logs.start();
        ((Logger) LoggerFactory.getLogger(Logger.ROOT_LOGGER_NAME)).addAppender(logs);
        User user = new User();
        user.setId(10L);
        ThreadContext.setCurrentUser(user);
        services.put(100L, mcp(100L));
    }

    @AfterEach
    void cleanup() throws Exception {
        for (McpClient client : clients) client.close();
        if (http != null) http.stop(0);
        if (context != null) context.close();
        ReflectionTestUtils.setField(SpringUtil.class, "applicationContext", null);
        ThreadContext.unload();
        AesUtil.AES_KEY = oldAesKey;
        if (logs != null) {
            ((Logger) LoggerFactory.getLogger(Logger.ROOT_LOGGER_NAME)).detachAppender(logs);
            logs.stop();
        }
    }

    @Test
    void encryptedEditIsPersistedAndSettingsDecryptionDoesNotMutateStoredValue() throws Exception {
        UserMcpDto first = service.saveOrUpdate(edit("u10-first", true));
        assertEquals("u10-first", first.getMcpCustomizedParams().get(0).getValue());
        assertTrue(stored.get(key(10L, 100L)).getMcpCustomizedParams().get(0).getEncrypted());
        UserMcpDto updated = service.saveOrUpdate(edit("u10-edited", true));
        UserMcp persisted = stored.get(key(10L, 100L));
        assertEquals("u10-edited", AesUtil.decrypt(String.valueOf(persisted.getMcpCustomizedParams().get(0).getValue())));
        assertEquals("u10-edited", updated.getMcpCustomizedParams().get(0).getValue());
        assertFalse(updated.getMcpCustomizedParams().get(0).getEncrypted());
        assertTrue(persisted.getMcpCustomizedParams().get(0).getEncrypted());
        clients.addAll(service.createMcpClients(10L, List.of(100L)));
        assertEquals(1, clients.size());
        assertTrue(requests.stream().filter(r -> r.method.equals("initialize")).allMatch(r -> r.authorization.equals("Bearer u10-edited") && !r.query.contains("u10-edited")));
        assertNoSecretsInLogs("u10-first", "u10-edited");
    }

    @Test
    void enableOnlyEditAndChangedEncryptionPolicyKeepThePersistedTokenUsable() throws Exception {
        service.saveOrUpdate(edit("u10-first", true));
        UserMcpUpdateReq disable = new UserMcpUpdateReq();
        disable.setMcpId(100L);
        disable.setIsEnable(false);
        UserMcpDto disabled = service.saveOrUpdate(disable);
        assertFalse(disabled.getIsEnable());
        assertFalse(stored.get(key(10L, 100L)).getIsEnable());
        assertTrue(service.createMcpClients(10L, List.of(100L)).isEmpty());
        assertTrue(requests.isEmpty());
        services.get(100L).getCustomizedParamDefinitions().get(0).setRequireEncrypt(false);
        disable.setIsEnable(true);
        UserMcpDto enabled = service.saveOrUpdate(disable);
        assertEquals("u10-first", enabled.getMcpCustomizedParams().get(0).getValue());
        assertTrue(stored.get(key(10L, 100L)).getMcpCustomizedParams().get(0).getEncrypted());
        clients.addAll(service.createMcpClients(10L, List.of(100L)));
        assertEquals(1, clients.size());
        assertTrue(requests.stream().allMatch(r -> r.authorization.equals("Bearer u10-first")));
        assertNoSecretsInLogs("u10-first");
    }

    @Test
    void encryptedPresetHeaderIsDecryptedForItsClientWithoutMutatingSharedService() throws Exception {
        Mcp mcp = services.get(100L);
        var fixed = mcp.getPresetParams().get(1);
        fixed.setValue(AesUtil.encrypt("fixture-fixed"));
        fixed.setEncrypted(true);
        stored.put(key(10L, 100L), storedUser(10L, 100L, "u10-first"));
        clients.addAll(service.createMcpClients(10L, List.of(100L)));
        assertEquals(1, clients.size());
        assertEquals("fixture-fixed", AesUtil.decrypt(String.valueOf(fixed.getValue())));
        assertTrue(fixed.getEncrypted());
        assertNoSecretsInLogs("u10-first", "fixture-fixed");
    }

    @Test
    void actualCharacterRegistrationToolProviderAndHostDispatchUseScopedSdkClient() throws Exception {
        service.saveOrUpdate(edit("u10-first", true));
        AiModel aiModel = new AiModel();
        aiModel.setIsReasoner(false);
        aiModel.setMaxInputTokens(1024);
        AbstractLLMService llm = mock(AbstractLLMService.class, CALLS_REAL_METHODS);
        ReflectionTestUtils.setField(llm, "aiModel", aiModel);
        doReturn(true).when(llm).isEnabled();
        ChatModel model = mock(ChatModel.class);
        doReturn(model).when(llm).buildChatLLM(any());
        AtomicInteger modelCalls = new AtomicInteger();
        when(model.chat(any(ChatRequest.class))).thenAnswer(i -> {
            ChatRequest req = i.getArgument(0);
            assertEquals("echo", req.toolSpecifications().get(0).name());
            int count = modelCalls.incrementAndGet();
            return ChatResponse.builder().aiMessage(count < 3
                    ? AiMessage.from(ToolExecutionRequest.builder().id("fixture-call").name("echo").arguments("{\"value\":9}").build())
                    : AiMessage.from("fixture completed")).build();
        });
        Character character = new Character();
        character.setUserId(10L);
        character.setMcpIds("100");
        character.setUnderstandContextEnable(false);
        ChatModelRequest request = CharacterChatHelper.buildChatRequestParams(character, "call the echo tool", ThreadContext.getCurrentUser(), llm, true, false, List.of());
        clients.addAll(request.getMcpClients());
        assertEquals(1, clients.size());
        ChatResponse response = llm.chat(SseAskParam.builder().uuid("fixture").httpRequestParams(request).build());
        assertEquals("fixture completed", response.aiMessage().text());
        assertEquals(1, calls.get());
        assertEquals(Boolean.TRUE, ReflectionTestUtils.getField(clients.get(0), "closed"));
        Object transport = ReflectionTestUtils.getField(clients.get(0), "transport");
        assertTrue(((java.util.concurrent.atomic.AtomicBoolean) ReflectionTestUtils.getField(transport, "closed")).get());
        assertEquals(0, deletes.get()); // This pinned SDK closes locally; it does not send session DELETE.
        assertTrue(requests.stream().allMatch(r -> !r.query.contains("u10-first")));
        assertNoSecretsInLogs("u10-first");
    }

    @Test
    void blockingChatToolEventsDoNotRequireAnSseId() {
        assertDoesNotThrow(() -> SseManager.sendToolCall(null, "echo", 1, true));
    }

    @Test
    void twoUsersAndServicesStayIsolatedAnd401DoesNotLogCredentials() throws Exception {
        services.put(200L, mcp(200L));
        stored.put(key(10L, 100L), storedUser(10L, 100L, "u10-first"));
        stored.put(key(20L, 100L), storedUser(20L, 100L, "u20-first"));
        stored.put(key(10L, 200L), storedUser(10L, 200L, "service-two"));
        clients.addAll(service.createMcpClients(10L, List.of(100L, 200L)));
        assertEquals(2, clients.size());
        List<McpClient> otherUser = service.createMcpClients(20L, List.of(100L));
        clients.addAll(otherUser);
        assertEquals(1, otherUser.size());
        assertTrue(requests.stream().anyMatch(r -> r.authorization.equals("Bearer u10-first") && r.query.contains("service=100")));
        assertTrue(requests.stream().anyMatch(r -> r.authorization.equals("Bearer service-two") && r.query.contains("service=200")));
        assertTrue(requests.stream().anyMatch(r -> r.authorization.equals("Bearer u20-first") && r.query.contains("service=100")));
        stored.put(key(10L, 100L), storedUser(10L, 100L, "denied-test-key"));
        assertTrue(service.createMcpClients(10L, List.of(100L)).isEmpty());
        assertNoSecretsInLogs("u10-first", "u20-first", "service-two", "denied-test-key");
    }

    @Test
    void missingBoundParameterFailsBeforeAnyNetworkRequest() throws Exception {
        UserMcp user = storedUser(10L, 100L, "u10-first");
        user.setMcpCustomizedParams(List.of());
        stored.put(key(10L, 100L), user);
        assertTrue(service.createMcpClients(10L, List.of(100L)).isEmpty());
        assertTrue(requests.isEmpty());
    }

    /** Inject a constructor failure to exercise secret-bearing exception messages and causes. */
    @ParameterizedTest
    @ValueSource(strings = {"ERROR", "DEBUG"})
    void buildFailureLogsExceptionClassWithoutMessagesOrCauses(String level) throws Exception {
        Logger logger = (Logger) LoggerFactory.getLogger(UserMcpService.class);
        Level previousLevel = logger.getLevel();
        logger.setLevel(Level.toLevel(level));
        stored.put(key(10L, 100L), storedUser(10L, 100L, "fixture-auth-value"));
        try (var builders = mockConstruction(DefaultMcpClient.Builder.class, (builder, ignored) -> {
            when(builder.transport(any())).thenReturn(builder);
            when(builder.build()).thenThrow(new IllegalStateException("fixture-url-secret",
                    new IllegalArgumentException("fixture-header-secret")));
        })) {
            assertTrue(service.createMcpClients(10L, List.of(100L)).isEmpty());
            List<ILoggingEvent> failures = logs.list.stream().filter(event ->
                    event.getLoggerName().equals(UserMcpService.class.getName())
                            && event.getFormattedMessage().startsWith("Failed to build MCP client")).toList();
            assertEquals(1, failures.size());
            ILoggingEvent failure = failures.get(0);
            assertEquals(Level.ERROR, failure.getLevel());
            assertTrue(failure.getFormattedMessage().contains(IllegalStateException.class.getName()));
            assertNull(failure.getThrowableProxy());
            assertNoSecretsInLogs("fixture-auth-value", "fixture-url-secret", "fixture-header-secret");
            assertTrue(requests.isEmpty());
        } finally {
            logger.setLevel(previousLevel);
        }
    }

    @Test
    void legacySseUsesTheSameHeadersForConnectionAndMessagesAndClosesItsExecutor() throws Exception {
        Mcp mcp = services.get(100L);
        mcp.setTransportType("sse");
        mcp.setSseUrl("http://127.0.0.1:" + http.getAddress().getPort() + "/sse");
        stored.put(key(10L, 100L), storedUser(10L, 100L, "u10-first"));
        clients.addAll(service.createMcpClients(10L, List.of(100L)));
        assertEquals(1, clients.size());
        McpClient client = clients.get(0);
        assertEquals("echo", client.listTools().get(0).name());
        assertEquals("fixture-tool-result", client.executeTool(ToolExecutionRequest.builder().id("fixture-sse-call").name("echo").arguments("{\"value\":9}").build()).resultText());
        assertTrue(requests.stream().allMatch(r -> r.authorization.equals("Bearer u10-first") && !r.query.contains("u10-first")));
        client.close();
        Object transport = ReflectionTestUtils.getField(client, "transport");
        Object httpClient = ReflectionTestUtils.getField(transport, "client");
        Object dispatcher = ReflectionTestUtils.invokeMethod(httpClient, "dispatcher");
        java.util.concurrent.ExecutorService executor = ReflectionTestUtils.invokeMethod(dispatcher, "executorService");
        assertTrue(executor.isShutdown());
        assertNoSecretsInLogs("u10-first");
    }

    private Mcp mcp(long id) throws Exception {
        String preset = "[{\"name\":\"service\",\"value\":\"" + id + "\"},{\"name\":\"fixed\",\"value\":\"fixture-fixed\",\"bind_type\":\"header\",\"bind_name\":\"X-Fixed\"}]";
        Mcp mcp = json.readValue("{\"id\":" + id + ",\"title\":\"fixture\",\"transportType\":\"streamable_http\",\"sseTimeout\":2,\"presetParams\":" + preset
                + ",\"customizedParamDefinitions\":[{\"name\":\"token\",\"require_encrypt\":true,\"bind_type\":\"header\",\"bind_name\":\"Authorization\",\"bind_value_template\":\"Bearer {value}\"}]}", Mcp.class);
        mcp.setSseUrl("http://127.0.0.1:" + http.getAddress().getPort() + "/mcp");
        return mcp;
    }

    private UserMcpUpdateReq edit(String token, boolean enabled) throws Exception {
        return json.readValue("{\"mcpId\":100,\"isEnable\":" + enabled + ",\"mcpCustomizedParams\":[{\"name\":\"token\",\"value\":\"" + token + "\",\"encrypted\":false}]}", UserMcpUpdateReq.class);
    }

    private UserMcp storedUser(long userId, long mcpId, String token) throws Exception {
        UserMcp user = json.readValue("{\"id\":" + (mcpId + userId) + ",\"userId\":" + userId + ",\"mcpId\":" + mcpId + ",\"isEnable\":true,\"isDeleted\":false,\"mcpCustomizedParams\":[{\"name\":\"token\",\"value\":\"" + AesUtil.encrypt(token) + "\",\"encrypted\":true}]}", UserMcp.class);
        return user;
    }

    private <T> T fixtureMapper(Class<T> type) {
        T delegate = mock(type);
        MybatisMapperProxy<T> handler = new MybatisMapperProxy<>(mock(SqlSession.class), type, new ConcurrentHashMap<>()) {
            @Override
            public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
                return method.invoke(delegate, args);
            }
        };
        return type.cast(Proxy.newProxyInstance(type.getClassLoader(), new Class<?>[]{type}, handler));
    }

    private <T> T copy(T value, Class<T> type) {
        return value == null ? null : json.convertValue(value, type);
    }

    private static String key(long userId, long mcpId) { return userId + ":" + mcpId; }

    private void assertNoSecretsInLogs(String... values) {
        String text = logs.list.stream().map(event -> event.getFormattedMessage() + (event.getThrowableProxy() == null ? "" : ThrowableProxyUtil.asString(event.getThrowableProxy()))).reduce("", String::concat);
        for (String value : values) assertFalse(text.contains(value), "A credential appeared in captured application/SDK logs");
    }

    private void handleSse(HttpExchange exchange) throws IOException {
        String authorization = exchange.getRequestHeaders().getFirst("Authorization");
        String query = exchange.getRequestURI().getRawQuery();
        requests.add(new Request("sse-connect", authorization == null ? "" : authorization, query == null ? "" : query));
        if (!"Bearer u10-first".equals(authorization) || !"fixture-fixed".equals(exchange.getRequestHeaders().getFirst("X-Fixed"))) {
            exchange.sendResponseHeaders(401, -1);
            exchange.close();
            return;
        }
        exchange.getResponseHeaders().set("Content-Type", "text/event-stream");
        exchange.sendResponseHeaders(200, 0);
        sseStream = exchange.getResponseBody();
        sseStream.write("event: endpoint\ndata: /messages?fixture=1\n\n".getBytes(StandardCharsets.UTF_8));
        sseStream.flush();
    }

    private void handleSseMessage(HttpExchange exchange) throws IOException {
        JsonNode request = json.readTree(exchange.getRequestBody());
        String authorization = exchange.getRequestHeaders().getFirst("Authorization");
        requests.add(new Request(request.path("method").asText(), authorization == null ? "" : authorization, exchange.getRequestURI().getRawQuery()));
        if (!"Bearer u10-first".equals(authorization) || !"fixture-fixed".equals(exchange.getRequestHeaders().getFirst("X-Fixed"))) {
            exchange.sendResponseHeaders(401, -1);
            exchange.close();
            return;
        }
        if (request.has("id")) {
            byte[] response = json.writeValueAsBytes(Map.of("jsonrpc", "2.0", "id", request.get("id"), "result", resultFor(request)));
            sseStream.write(("event: message\ndata: " + new String(response, StandardCharsets.UTF_8) + "\n\n").getBytes(StandardCharsets.UTF_8));
            sseStream.flush();
        }
        exchange.sendResponseHeaders(202, -1);
        exchange.close();
    }

    private void handle(HttpExchange exchange) throws IOException {
        if (exchange.getRequestMethod().equals("GET")) { exchange.sendResponseHeaders(405, -1); exchange.close(); return; }
        String authorization = exchange.getRequestHeaders().getFirst("Authorization");
        String query = exchange.getRequestURI().getRawQuery();
        if (exchange.getRequestMethod().equals("DELETE")) { deletes.incrementAndGet(); exchange.sendResponseHeaders(200, -1); exchange.close(); return; }
        JsonNode request = json.readTree(exchange.getRequestBody());
        requests.add(new Request(request.path("method").asText(), authorization == null ? "" : authorization, query == null ? "" : query));
        if (authorization == null || authorization.contains("denied-test-key") || !"fixture-fixed".equals(exchange.getRequestHeaders().getFirst("X-Fixed"))) {
            exchange.sendResponseHeaders(401, -1); exchange.close(); return;
        }
        if (!request.has("id")) { exchange.sendResponseHeaders(202, -1); exchange.close(); return; }
        if (request.path("method").asText().equals("initialize")) {
            exchange.getResponseHeaders().set("Mcp-Session-Id", "fixture-session");
        }
        Object result = resultFor(request);
        byte[] response = json.writeValueAsBytes(Map.of("jsonrpc", "2.0", "id", request.get("id"), "result", result));
        exchange.getResponseHeaders().set("Content-Type", "application/json");
        exchange.sendResponseHeaders(200, response.length);
        exchange.getResponseBody().write(response);
        exchange.close();
    }

    private Object resultFor(JsonNode request) {
        Object result;
        switch (request.path("method").asText()) {
            case "initialize" -> {
                result = Map.of("protocolVersion", request.path("params").path("protocolVersion").asText(), "capabilities", Map.of("tools", Map.of()), "serverInfo", Map.of("name", "synthetic-fixture", "version", "1"));
            }
            case "tools/list" -> result = Map.of("tools", List.of(Map.of("name", "echo", "description", "Synthetic fixture", "inputSchema", Map.of("type", "object", "properties", Map.of("value", Map.of("type", "integer")), "required", List.of("value")))));
            case "tools/call" -> {
                assertEquals(9, request.path("params").path("arguments").path("value").asInt());
                calls.incrementAndGet();
                result = Map.of("content", List.of(Map.of("type", "text", "text", "fixture-tool-result")), "isError", false);
            }
            default -> result = Map.of();
        }
        return result;
    }

    private record Request(String method, String authorization, String query) { }
}
