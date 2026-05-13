package com.moyz.adi.common.workflow.node.keywordextractor;

public class KeywordExtractorPrompt {

    public static String getPrompt(int topN, String userQuestion) {
        return """
                - Role: You're a question analyzer.
                - Requirements:
                  - Summarize user's question, and give top %d important keyword.
                  - Use comma as a delimiter to separate keywords.
                - Answer format: (in language of user's question)
                - keyword example: a, b, c
                
                ### User question: %s
                """.formatted(topN, userQuestion);
    }

}
