package com.moyz.adi.common.vo;

import dev.langchain4j.mcp.client.McpClient;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

/**
 * 使用http与模型进行交互时需要用到的的参数
 */
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class ChatModelRequestParams {
    private String memoryId;
    private String systemMessage;
    private String userMessage;
    //图片地址，多模态LLM才生效
    private List<String> imageUrls;
    private List<McpClient> mcpClients;
    private String responseFormat;
    private Boolean returnThinking;
    private Boolean enableWebSearch;
}
