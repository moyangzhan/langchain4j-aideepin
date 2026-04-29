package com.moyz.adi.common.languagemodel;

import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.entity.AiModel;
import com.moyz.adi.common.entity.ModelPlatform;
import com.moyz.adi.common.util.LocalCache;
import com.moyz.adi.common.vo.ChatModelBuilderProperties;
import dev.langchain4j.model.chat.ChatModel;
import dev.langchain4j.model.chat.StreamingChatModel;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.EnabledIfEnvironmentVariable;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Integration tests for MinimaxLLMService.
 * These tests require a valid MINIMAX_API_KEY environment variable.
 */
@EnabledIfEnvironmentVariable(named = "MINIMAX_API_KEY", matches = ".+")
class MinimaxLLMServiceIntegrationTest {

    private MinimaxLLMService service;
    private ChatModelBuilderProperties builderProperties;

    @BeforeEach
    void setUp() {
        // Populate required TTS config in LocalCache so AbstractLLMService constructor succeeds
        LocalCache.CONFIGS.put(AdiConstant.SysConfigKey.TTS_SETTING,
                "{\"synthesizer_side\":\"client\",\"model_name\":\"cosyvoice-v2\",\"platform\":\"dashscope\"}");

        ModelPlatform modelPlatform = new ModelPlatform();
        modelPlatform.setId(1L);
        modelPlatform.setName(AdiConstant.ModelPlatform.MINIMAX);
        modelPlatform.setTitle("MiniMax");
        modelPlatform.setBaseUrl("https://api.minimax.io/v1");
        modelPlatform.setApiKey(System.getenv("MINIMAX_API_KEY"));
        modelPlatform.setIsProxyEnable(false);
        modelPlatform.setIsOpenaiApiCompatible(false);

        AiModel aiModel = new AiModel();
        aiModel.setId(1L);
        aiModel.setName("MiniMax-M2.5-highspeed");
        aiModel.setTitle("MiniMax-M2.5-highspeed");
        aiModel.setType(AdiConstant.ModelType.TEXT);
        aiModel.setPlatform(AdiConstant.ModelPlatform.MINIMAX);
        aiModel.setIsEnable(true);
        aiModel.setContextWindow(204000);
        aiModel.setMaxInputTokens(180000);
        aiModel.setMaxOutputTokens(24000);
        aiModel.setResponseFormatTypes("text,json_object");

        service = new MinimaxLLMService(aiModel, modelPlatform);

        builderProperties = new ChatModelBuilderProperties();
        builderProperties.setTemperature(0.7);
    }

    @Test
    void testBuildChatModel() {
        ChatModel chatModel = service.buildChatLLM(builderProperties);
        assertNotNull(chatModel);
    }

    @Test
    void testBuildStreamingChatModel() {
        StreamingChatModel streamingChatModel = service.buildStreamingChatModel(builderProperties);
        assertNotNull(streamingChatModel);
    }

    @Test
    void testChatModelResponse() {
        ChatModel chatModel = service.buildChatLLM(builderProperties);
        String response = chatModel.chat("Say hello in one word");
        assertNotNull(response);
        assertFalse(response.isEmpty());
    }
}
