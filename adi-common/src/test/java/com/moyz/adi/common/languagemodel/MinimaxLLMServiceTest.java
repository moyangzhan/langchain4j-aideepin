package com.moyz.adi.common.languagemodel;

import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.entity.AiModel;
import com.moyz.adi.common.entity.ModelPlatform;
import com.moyz.adi.common.util.LocalCache;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Unit tests for MinimaxLLMService
 */
class MinimaxLLMServiceTest {

    private AiModel aiModel;
    private ModelPlatform modelPlatform;

    @BeforeEach
    void setUp() {
        // Populate required TTS config in LocalCache so AbstractLLMService constructor succeeds
        LocalCache.CONFIGS.put(AdiConstant.SysConfigKey.TTS_SETTING,
                "{\"synthesizer_side\":\"client\",\"model_name\":\"cosyvoice-v2\",\"platform\":\"dashscope\"}");

        modelPlatform = new ModelPlatform();
        modelPlatform.setId(1L);
        modelPlatform.setName(AdiConstant.ModelPlatform.MINIMAX);
        modelPlatform.setTitle("MiniMax");
        modelPlatform.setBaseUrl("https://api.minimax.io/v1");
        modelPlatform.setApiKey("test-api-key");
        modelPlatform.setIsProxyEnable(false);
        modelPlatform.setIsOpenaiApiCompatible(false);

        aiModel = new AiModel();
        aiModel.setId(1L);
        aiModel.setName("MiniMax-M2.7");
        aiModel.setTitle("MiniMax-M2.7");
        aiModel.setType(AdiConstant.ModelType.TEXT);
        aiModel.setPlatform(AdiConstant.ModelPlatform.MINIMAX);
        aiModel.setIsEnable(true);
        aiModel.setContextWindow(1000000);
        aiModel.setMaxInputTokens(900000);
        aiModel.setMaxOutputTokens(100000);
        aiModel.setResponseFormatTypes("text,json_object");
    }

    @Test
    void testMinimaxLLMServiceCreation() {
        MinimaxLLMService service = new MinimaxLLMService(aiModel, modelPlatform);
        assertNotNull(service);
    }

    @Test
    void testMinimaxLLMServiceIsEnabled() {
        MinimaxLLMService service = new MinimaxLLMService(aiModel, modelPlatform);
        assertTrue(service.isEnabled());
    }

    @Test
    void testMinimaxLLMServiceIsDisabledWithoutApiKey() {
        modelPlatform.setApiKey("");
        MinimaxLLMService service = new MinimaxLLMService(aiModel, modelPlatform);
        assertFalse(service.isEnabled());
    }

    @Test
    void testMinimaxLLMServiceIsDisabledWithNullApiKey() {
        modelPlatform.setApiKey(null);
        MinimaxLLMService service = new MinimaxLLMService(aiModel, modelPlatform);
        assertFalse(service.isEnabled());
    }

    @Test
    void testMinimaxLLMServiceIsDisabledWhenModelDisabled() {
        aiModel.setIsEnable(false);
        MinimaxLLMService service = new MinimaxLLMService(aiModel, modelPlatform);
        assertFalse(service.isEnabled());
    }

    @Test
    void testMinimaxLLMServiceExtendsOpenAiLLMService() {
        MinimaxLLMService service = new MinimaxLLMService(aiModel, modelPlatform);
        assertInstanceOf(OpenAiLLMService.class, service);
    }

    @Test
    void testMinimaxLLMServiceExtendsAbstractLLMService() {
        MinimaxLLMService service = new MinimaxLLMService(aiModel, modelPlatform);
        assertInstanceOf(AbstractLLMService.class, service);
    }

    @Test
    void testMinimaxLLMServiceSetProxyAddressChaining() {
        MinimaxLLMService service = new MinimaxLLMService(aiModel, modelPlatform);
        AbstractLLMService result = service.setProxyAddress(null);
        assertSame(service, result);
    }

    @Test
    void testMinimaxLLMServiceTokenEstimator() {
        MinimaxLLMService service = new MinimaxLLMService(aiModel, modelPlatform);
        // MiniMax uses OpenAI-compatible token estimator (falls back to GPT-3.5 turbo)
        assertNotNull(service.getTokenEstimator());
    }

    @Test
    void testMinimaxLLMServiceWithM25Model() {
        aiModel.setName("MiniMax-M2.5");
        aiModel.setTitle("MiniMax-M2.5");
        MinimaxLLMService service = new MinimaxLLMService(aiModel, modelPlatform);
        assertTrue(service.isEnabled());
    }

    @Test
    void testMinimaxLLMServiceWithM25HighspeedModel() {
        aiModel.setName("MiniMax-M2.5-highspeed");
        aiModel.setTitle("MiniMax-M2.5-highspeed");
        aiModel.setContextWindow(204000);
        aiModel.setMaxInputTokens(180000);
        aiModel.setMaxOutputTokens(24000);
        MinimaxLLMService service = new MinimaxLLMService(aiModel, modelPlatform);
        assertTrue(service.isEnabled());
    }

    @Test
    void testModelPlatformMinimaxConstant() {
        assertEquals("minimax", AdiConstant.ModelPlatform.MINIMAX);
    }

    @Test
    void testModelPlatformConstantsContainMinimax() {
        assertTrue(AdiConstant.ModelPlatform.getModelConstants().contains("minimax"));
    }
}
