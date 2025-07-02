package com.moyz.adi.common.vo;

import dev.langchain4j.mcp.client.McpClient;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

/**
 * llm使用到的信息
 */
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class ChatModelParams {
    private String memoryId;
    private String systemMessage;
    private String userMessage;
    //图片地址，多模态LLM才生效
    private List<String> imageUrls;
    private List<McpClient> mcpClients;
}
