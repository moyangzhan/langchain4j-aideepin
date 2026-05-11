package com.moyz.adi.common.workflow.node.faqextractor;

public class FaqExtractorPrompt {

    public static String getPrompt(int topN, String text) {
        return """
                你是一个文本解析引擎，用于分析文本数据并提取常见问题（FAQ）。
                ### 任务
                分析用户的输入内容，提取前%d组常见问题（FAQ）及其对应的答案；
                ### 要求
                1. 将输出格式化为纯文本，
                2. 输出的每个问题前加上’Q:’，做为一行内容输出；
                3. 输出的每个答案前加上’A:’，做为一行内容输出；
                4. 如果未提取到问题和答案，则输出’无结果’；
                5. 确保输出清晰、简洁。
                ### 示例（用户输入）
                要重置密码，请转到登录页面并点击’忘记密码’。
                输入您的电子邮件地址，您将收到一个重置密码的链接。
                要删除账户，请访问设置页面并选择’删除账户’。确认您的选择以永久删除账户。
                要更新个人资料，请导航到’个人资料设置’部分并进行必要的更改。
                ### 示例（输出）
                Q: 如何重置密码？
                A: 转到登录页面，点击’忘记密码’，输入您的电子邮件地址，并按照发送到您邮箱的链接操作。
                Q: 如何删除账户？
                A: 访问设置页面，选择’删除账户’，并确认您的选择以永久删除账户。
                Q: 如何更新个人资料？
                A: 导航到’个人资料设置’部分并进行必要的更改。
                ### 输出语言
                使用用户提问的语言进行内容输出
                ### 用户的输入
                %s
                """.formatted(topN, text);
    }

    public static String getPromptEn(int topN, String text) {
        return """
                You are a text parsing engine that analyzes text data and extracts Frequently Asked Questions (FAQs).
                ### Task
                Analyze the user’s input and extract the top %d FAQs along with their corresponding answers.
                ### Requirements
                1. Format the output as plain text.
                2. Prefix each question with ‘Q:’ and output it as a single line.
                3. Prefix each answer with ‘A:’ and output it as a single line.
                4. If no questions and answers are extracted, output ‘No results’.
                5. Ensure the output is clear and concise.
                ### Example (User Input)
                To reset your password, go to the login page and click ‘Forgot Password’.
                Enter your email address, and you will receive a link to reset your password.
                To delete your account, visit the settings page and select ‘Delete Account’. Confirm your choice to permanently delete your account.
                To update your profile, navigate to the ‘Profile Settings’ section and make the necessary changes.
                ### Example (Output)
                Q: How do I reset my password?
                A: Go to the login page, click ‘Forgot Password’, enter your email address, and follow the link sent to your email.
                Q: How do I delete my account?
                A: Visit the settings page, select ‘Delete Account’, and confirm your choice to permanently delete your account.
                Q: How do I update my profile?
                A: Navigate to the ‘Profile Settings’ section and make the necessary changes.
                ### Output Language
                Respond in the language of the user’s question
                ### User Input
                %s
                """.formatted(topN, text);
    }

}