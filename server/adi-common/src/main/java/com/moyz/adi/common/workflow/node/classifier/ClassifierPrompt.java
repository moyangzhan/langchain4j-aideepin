package com.moyz.adi.common.workflow.node.classifier;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.moyz.adi.common.util.JsonUtil;
import lombok.Data;
import org.springframework.beans.BeanUtils;

import java.util.List;

public class ClassifierPrompt {
    public static String createPrompt(String input, List<ClassifierCategory> categories) {
        List<PromptCategory> promptCategories = categories.stream().map(item -> {
            PromptCategory promptCategory = new PromptCategory();
            BeanUtils.copyProperties(item, promptCategory);
            return promptCategory;
        }).toList();
        return """
                ### Job Description
                You are a text classification engine that analyzes text data and assigns categories based on user input or automatically determined categories.
                ### Task
                Your task is to assign one categories ONLY to the input text and only one category may be assigned returned in the output. Additionally, you need to extract the key words from the text that are related to the classification.
                ### Format
                The input text is in the variable input_text. Categories are specified as a category list  with two filed category_uuid and category_name in the variable categories. Classification instructions may be included to improve the classification accuracy.
                ### Constraint
                DO NOT include anything other than the JSON array in your response.
                ### Example
                Here is the chat example between human and assistant, inside <example></example> XML tags.
                <example>
                User:{"input_text": ["I recently had a great experience with your company. The service was prompt and the staff was very friendly."], "categories": [{"category_uuid":"f5660049-284f-41a7-b301-fd24176a711c","category_name":"Customer Service"},{"category_uuid":"8d007d06-f2c9-4be5-8ff6-cd4381c13c60","category_name":"Satisfaction"},{"category_uuid":"5fbbbb18-9843-466d-9b8e-b9bfbb9482c8","category_name":"Sales"},{"category_uuid":"23623c75-7184-4a2e-8226-466c2e4631e4","category_name":"Product"}]}
                Assistant:{"keywords": ["recently", "great experience", "company", "service", "prompt", "staff", "friendly"],"category_uuid": "f5660049-284f-41a7-b301-fd24176a711c","category_name": "Customer Service"}
                User:{"input_text": ["bad service, slow to bring the food"], "categories": [{"category_uuid":"80fb86a0-4454-4bf5-924c-f253fdd83c02","category_name":"Food Quality"},{"category_uuid":"f6ff5bc3-aca0-4e4a-8627-e760d0aca78f","category_name":"Experience"},{"category_uuid":"cc771f63-74e7-4c61-882e-3eda9d8ba5d7","category_name":"Price"}]}
                Assistant:{"keywords": ["bad service", "slow", "food", "tip", "terrible", "waitresses"],"category_uuid": "f6ff5bc3-aca0-4e4a-8627-e760d0aca78f","category_name": "Experience"}
                </example>
                ### User Input
                {"input_text" : ["%s"], "categories" : %s}
                ### Assistant Output
                """.formatted(input, JsonUtil.toJson(promptCategories));
    }

    @Data
    public static class PromptCategory {
        @JsonProperty("category_uuid")
        private String categoryUuid;
        @JsonProperty("category_name")
        private String categoryName;
    }
}
