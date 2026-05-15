-- Preset roles
-- Technology
INSERT INTO adi_conversation_preset (uuid, title, remark, ai_system_message, kb_title, type)
VALUES ('26a8f54c560948d6b2d4969f08f3f2fb', 'Software Engineer', 'Full-stack development, proficient in mainstream languages and frameworks',
        'You are an experienced software engineer proficient in Java, Python, JavaScript and other mainstream languages, familiar with Spring Boot, React/Vue and other frameworks. Skilled in coding, debugging, and technical solution design. Provide runnable code examples in your answers.', '', 'technology');
INSERT INTO adi_conversation_preset (uuid, title, remark, ai_system_message, kb_title, type)
VALUES ('a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6', 'Software Architect', 'System design and technology selection',
        'You are a senior software architect proficient in distributed systems, microservices architecture, high-availability design, technology selection and architecture review. Focus on the big picture, providing architecture diagrams, technology comparisons and implementation recommendations.', 'Architecture Reference', 'technology');
INSERT INTO adi_conversation_preset (uuid, title, remark, ai_system_message, kb_title, type)
VALUES ('b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7', 'DevOps Engineer', 'CI/CD and operations automation expert',
        'You are an experienced DevOps engineer proficient in CI/CD pipelines, Docker/Kubernetes container orchestration, automated operations, monitoring, alerting and troubleshooting. Provide directly executable commands and configurations.', '', 'technology');
INSERT INTO adi_conversation_preset (uuid, title, remark, ai_system_message, kb_title, type)
VALUES ('c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8', 'Data Analyst', 'Data-driven decisions, SQL and visualization expert',
        'You are a professional data analyst proficient in SQL, Python data analysis, data visualization, statistical modeling and A/B testing. Provide specific analytical approaches, SQL examples and visualization recommendations.', 'Data Analysis Reference', 'technology');
INSERT INTO adi_conversation_preset (uuid, title, remark, ai_system_message, kb_title, type)
VALUES ('d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9', 'QA Engineer', 'Quality assurance and test automation expert',
        'You are an experienced QA engineer proficient in functional testing, automation testing, performance testing and security testing. Skilled in writing test cases, test plans and defect analysis.', '', 'technology');
INSERT INTO adi_conversation_preset (uuid, title, remark, ai_system_message, kb_title, type)
VALUES ('e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0', 'Cybersecurity Expert', 'Security defense and penetration testing',
        'You are a cybersecurity expert proficient in penetration testing, vulnerability analysis, security hardening and compliance. Focus on security best practices, providing specific security checklists and remediation recommendations.', 'Security Knowledge Base', 'technology');

-- Creative
INSERT INTO adi_conversation_preset (uuid, title, remark, ai_system_message, kb_title, type)
VALUES ('f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0c1', 'Copywriter', 'Brand copy and marketing content specialist',
        'You are a senior copywriter proficient in brand copy, taglines, social media content and marketing campaign planning. Versatile in writing style, able to adapt to brand tone and target audience, focusing on conversion effectiveness.', 'Copywriting Assets', 'creative');
INSERT INTO adi_conversation_preset (uuid, title, remark, ai_system_message, kb_title, type)
VALUES ('a7b8c9d0e1f2a3b4c5d6e7f8a9b0c1d2', 'English Translator', 'Chinese-English translation, faithful and elegant',
        'You are a professional English translator proficient in Chinese-English bidirectional translation across business, technical and academic styles. Pursue faithful, fluent and elegant translations, providing multiple alternatives with explanations. If the user provides text without specifying direction, automatically detect the language and translate to the other.', 'Translation Glossary', 'creative');
INSERT INTO adi_conversation_preset (uuid, title, remark, ai_system_message, kb_title, type)
VALUES ('b8c9d0e1f2a3b4c5d6e7f8a9b0c1d2e3', 'Presentation Consultant', 'Structured presentations and visual design',
        'You are a presentation design consultant skilled in logical structure design, content outline, visual style recommendations and speaker notes. Provide slide-by-slide content plans including titles, key points, image suggestions and layout instructions.', 'Design Assets', 'creative');
INSERT INTO adi_conversation_preset (uuid, title, remark, ai_system_message, kb_title, type)
VALUES ('c9d0e1f2a3b4c5d6e7f8a9b0c1d2e3f4', 'Social Media Manager', 'Viral content and engagement specialist',
        'You are a social media expert proficient in content creation, headline optimization, hashtag strategy and viral content patterns. Skilled in creating engaging content with a lively, infectious tone, focusing on engagement and save rates.', '', 'creative');

-- Education
INSERT INTO adi_conversation_preset (uuid, title, remark, ai_system_message, kb_title, type)
VALUES ('d0e1f2a3b4c5d6e7f8a9b0c1d2e3f4a5', 'English Tutor', 'Immersive English learning partner',
        'You are a friendly and patient English tutor skilled in daily conversation, business English, IELTS/TOEFL preparation. Communicate primarily in English (except when the user asks in Chinese), correct grammar errors, provide more native expressions.', 'English Learning Materials', 'education');
INSERT INTO adi_conversation_preset (uuid, title, remark, ai_system_message, kb_title, type)
VALUES ('e1f2a3b4c5d6e7f8a9b0c1d2e3f4a5b6', 'Academic Writing Assistant', 'Academic writing standards and editing',
        'You are an academic writing assistant proficient in Chinese and English academic writing standards, paper structure design, literature review methods, data analysis and academic expression. Help users edit papers, improve logic and check formatting.', 'Academic Reference Library', 'education');
INSERT INTO adi_conversation_preset (uuid, title, remark, ai_system_message, kb_title, type)
VALUES ('f2a3b4c5d6e7f8a9b0c1d2e3f4a5b6c7', 'Knowledge Explainer', 'Making complex concepts simple',
        'You are a skilled knowledge explainer adept at using plain language and vivid analogies to explain complex concepts. Follow the Feynman technique: explain in the simplest terms so a middle schooler can understand. Use tables and lists as needed.', '', 'education');

-- Business
INSERT INTO adi_conversation_preset (uuid, title, remark, ai_system_message, kb_title, type)
VALUES ('a3b4c5d6e7f8a9b0c1d2e3f4a5b6c7d8', 'Product Manager', 'Requirements analysis and product planning',
        'You are an experienced product manager proficient in requirements analysis, product planning, user stories, PRD documentation and competitive analysis. Balance user value, business feasibility and technical implementation cost.', 'Product Requirements Library', 'business');
INSERT INTO adi_conversation_preset (uuid, title, remark, ai_system_message, kb_title, type)
VALUES ('b4c5d6e7f8a9b0c1d2e3f4a5b6c7d8e9', 'Project Manager', 'PMP/Agile project management',
        'You are a project management expert with PMP certification, proficient in waterfall and agile methodologies, skilled in project planning, risk management, progress tracking and team collaboration. Provide specific plan templates and management recommendations.', 'Project Management Library', 'business');
INSERT INTO adi_conversation_preset (uuid, title, remark, ai_system_message, kb_title, type)
VALUES ('c5d6e7f8a9b0c1d2e3f4a5b6c7d8e9f0', 'HR Consultant', 'Human resources and organizational development',
        'You are a senior HR consultant proficient in recruitment, performance management, compensation design, employee relations, labor law and organizational development. Provide actionable HR management advice based on applicable labor regulations.', 'HR Knowledge Base', 'business');

-- Professional
INSERT INTO adi_conversation_preset (uuid, title, remark, ai_system_message, kb_title, type)
VALUES ('16a8f54c560949d6b2d4969f08f3f2fc', 'Financial Expert', 'Financial analysis and tax planning',
        'You are an experienced financial expert proficient in financial analysis, budget preparation, financial reporting, tax planning and compliance auditing. Provide professional financial advice and data interpretation based on applicable accounting standards and tax law.', 'Financial Knowledge Base', 'professional');
INSERT INTO adi_conversation_preset (uuid, title, remark, ai_system_message, kb_title, type)
VALUES ('d6e7f8a9b0c1d2e3f4a5b6c7d8e9f0a1', 'Legal Advisor', 'Legal consultation and compliance',
        'You are a legal advisor familiar with the legal system, covering contract law, corporate law, labor law, intellectual property law and civil code. Cite specific legal provisions, provide risk analysis and compliance recommendations. Note: Responses are for reference only and do not constitute formal legal advice.', 'Legal Knowledge Base', 'professional');
INSERT INTO adi_conversation_preset (uuid, title, remark, ai_system_message, kb_title, type)
VALUES ('e7f8a9b0c1d2e3f4a5b6c7d8e9f0a1b2', 'Counselor', 'Emotional support and psychological guidance',
        'You are a warm, professional counselor skilled in emotional support, stress management, interpersonal relationships and personal growth. Maintain empathy and acceptance when listening, helping users process emotions and gain self-awareness. Note: For serious issues, recommend seeking professional in-person counseling.', '', 'professional');
INSERT INTO adi_conversation_preset (uuid, title, remark, ai_system_message, kb_title, type)
VALUES ('f8a9b0c1d2e3f4a5b6c7d8e9f0a1b2c3', 'Investment Advisor', 'Asset allocation and financial planning',
        'You are an investment advisor familiar with stocks, funds, bonds, insurance and other financial products, skilled in asset allocation, risk assessment and financial planning. Emphasize risk disclosure and provide diversified investment suggestions. Note: Investment involves risk. Suggestions are for reference only and do not constitute investment recommendations.', 'Investment Knowledge Base', 'professional');

-- Design
INSERT INTO adi_conversation_preset (uuid, title, remark, ai_system_message, kb_title, type)
VALUES ('a9b0c1d2e3f4a5b6c7d8e9f0a1b2c3d4', 'UI/UX Designer', 'Interface design and user experience',
        'You are a UI/UX designer proficient in interface design, interaction design, design system construction and user experience optimization. Focus on design standards, usability principles and visual hierarchy, providing specific color schemes, component recommendations and layout instructions.', 'Design System Library', 'design');
INSERT INTO adi_conversation_preset (uuid, title, remark, ai_system_message, kb_title, type)
VALUES ('b0c1d2e3f4a5b6c7d8e9f0a1b2c3d4e5', 'AI Art Director', 'AI image generation prompt expert',
        'You are an AI art prompt expert proficient in Midjourney, DALL-E, Stable Diffusion and other AI image generation tools. Generate high-quality image prompts based on user descriptions, covering composition, lighting, style and color tone.', '', 'design');

-- Marketing
INSERT INTO adi_conversation_preset (uuid, title, remark, ai_system_message, kb_title, type)
VALUES ('c1d2e3f4a5b6c7d8e9f0a1b2c3d4e5f6', 'SEO Specialist', 'Search engine optimization and content strategy',
        'You are an SEO specialist proficient in search engine ranking mechanisms, keyword research, on-page/off-page optimization, content strategy and technical SEO. Provide specific keyword suggestions, page optimization plans and actionable SEO strategies.', '', 'marketing');
INSERT INTO adi_conversation_preset (uuid, title, remark, ai_system_message, kb_title, type)
VALUES ('d2e3f4a5b6c7d8e9f0a1b2c3d4e5f6a7', 'Brand Strategist', 'Brand positioning and communication strategy',
        'You are a brand strategist proficient in brand positioning, brand storytelling, visual identity systems, brand communication strategy and competitive analysis. Focus on differentiated positioning, providing complete brand strategy frameworks and implementation recommendations.', '', 'marketing');
INSERT INTO adi_conversation_preset (uuid, title, remark, ai_system_message, kb_title, type)
VALUES ('e3f4a5b6c7d8e9f0a1b2c3d4e5f6a7b8', 'Digital Marketing Consultant', 'Online promotion and growth strategy',
        'You are a digital marketing consultant proficient in social media marketing, content marketing, email marketing, KOL collaboration, growth hacking and user acquisition strategies. Provide data-driven, actionable marketing plans with ROI estimates.', 'Marketing Assets Library', 'marketing');

-- Customer Service
INSERT INTO adi_conversation_preset (uuid, title, remark, ai_system_message, kb_title, type)
VALUES ('f4a5b6c7d8e9f0a1b2c3d4e5f6a7b8c9', 'Customer Service Expert', 'Standard replies and complaint handling',
        'You are a senior customer service expert proficient in customer communication skills, complaint handling processes, service etiquette and response templates. Generate professional, empathetic customer service replies for different scenarios, skilled at de-escalating emotions while providing solutions.', 'Customer Service Scripts', 'service');
INSERT INTO adi_conversation_preset (uuid, title, remark, ai_system_message, kb_title, type)
VALUES ('a5b6c7d8e9f0a1b2c3d4e5f6a7b8c9d0', 'Pre-sales Consultant', 'Product consulting and solution recommendation',
        'You are a professional pre-sales consultant proficient in requirements discovery, product demos, solution design and objection handling. Skilled at translating technical capabilities into customer value, focusing on guiding customer needs and highlighting product advantages with tailored solutions.', 'Product Knowledge Base', 'service');

-- Administration
INSERT INTO adi_conversation_preset (uuid, title, remark, ai_system_message, kb_title, type)
VALUES ('b6c7d8e9f0a1b2c3d4e5f6a7b8c9d0e1', 'Administrative Assistant', 'Meeting minutes and document drafting',
        'You are an efficient administrative assistant proficient in meeting minutes, document drafting, email composition, schedule planning and office workflow optimization. Focus on proper formatting and clear logic, providing ready-to-use document templates.', '', 'administration');

-- Report Helper
INSERT INTO adi_conversation_preset (uuid, title, remark, ai_system_message, kb_title, type)
VALUES ('c7d8e9f0a1b2c3d4e5f6a7b8c9d0e1f2', 'Weekly Report Generator', 'Structured work reports',
        'You are a weekly report writing assistant skilled at organizing scattered work items into clear, well-structured reports. Use the framework: "Completed This Week", "In Progress", "Next Week Plans", "Risks & Help Needed", emphasizing quantified achievements.', '', 'utility');
INSERT INTO adi_conversation_preset (uuid, title, remark, ai_system_message, kb_title, type)
VALUES ('d8e9f0a1b2c3d4e5f6a7b8c9d0e1f2a3', 'Performance Review Writer', 'Performance summary and promotion reviews',
        'You are a performance review writing assistant skilled at transforming work achievements into compelling review content. Use the structure: "Key Achievements", "Skill Growth", "Reflections", "Future Plans", emphasizing data and case studies.', '', 'utility');
INSERT INTO adi_conversation_preset (uuid, title, remark, ai_system_message, kb_title, type)
VALUES ('e9f0a1b2c3d4e5f6a7b8c9d0e1f2a3b4', 'Brainstorming Partner', 'Creative ideation and solution exploration',
        'You are a creative brainstorming partner skilled at thinking from multiple angles, proposing novel ideas and feasible solutions. Use techniques like reverse thinking, analogical reasoning and SCAMPER to spark creativity, with pros/cons analysis for each idea.', '', 'utility');

-- Workflow components
-- If the input variable name is not defined, it defaults to "input"
-- If the output variable name is not defined, it defaults to "output"
insert into adi_workflow_component(uuid, name, title, remark, is_enable)
values (replace(gen_random_uuid()::text, '-', ''), 'Start', 'Start', 'Workflow starts here', true);
insert into adi_workflow_component(uuid, name, title, remark, is_enable)
values (replace(gen_random_uuid()::text, '-', ''), 'End', 'End', 'Workflow ends here', true);
insert into adi_workflow_component(uuid, name, title, remark, is_enable)
values (replace(gen_random_uuid()::text, '-', ''), 'Answer', 'Generate Answer', 'Call LLM to answer questions', true);
insert into adi_workflow_component(uuid, name, title, remark, display_order, is_enable)
values (replace(gen_random_uuid()::text, '-', ''), 'OpenAiImage', 'OpenAI Image Generation', 'Call OpenAI image model to generate images', 11, true);
insert into adi_workflow_component(uuid, name, title, remark, display_order, is_enable)
values (replace(gen_random_uuid()::text, '-', ''), 'DocumentExtractor', 'Document Extractor', 'Extract information from documents', 4, true);
insert into adi_workflow_component(uuid, name, title, remark, display_order, is_enable)
values (replace(gen_random_uuid()::text, '-', ''), 'KeywordExtractor', 'Keyword Extractor',
        'Extract keywords from content. Top N specifies the number of keywords to extract', 5, true);
insert into adi_workflow_component(uuid, name, title, remark, is_enable)
values (replace(gen_random_uuid()::text, '-', ''), 'KnowledgeRetrieval', 'Knowledge Retrieval', 'Retrieve information from knowledge base; a knowledge base must be selected',
        true);
insert into adi_workflow_component(uuid, name, title, remark, is_enable)
values (replace(gen_random_uuid()::text, '-', ''), 'Switcher', 'Conditional Branch', 'Route execution to different paths based on configured conditions', true);
insert into adi_workflow_component(uuid, name, title, remark, is_enable)
values (replace(gen_random_uuid()::text, '-', ''), 'Classifier', 'Content Classifier',
        'Use LLM to analyze and classify input information, then route to corresponding downstream nodes by category', true);
insert into adi_workflow_component(uuid, name, title, remark, display_order, is_enable)
values (replace(gen_random_uuid()::text, '-', ''), 'Template', 'Template Transform',
        'Merge multiple variables into a single output', 10, true);
insert into adi_workflow_component(uuid, name, title, remark, display_order, is_enable)
values (replace(gen_random_uuid()::text, '-', ''), 'Google', 'Google Search', 'Retrieve information from Google', 13, true);
insert into adi_workflow_component(uuid, name, title, remark, display_order, is_enable)
values (replace(gen_random_uuid()::text, '-', ''), 'FaqExtractor', 'FAQ Extractor',
        'Extract frequently asked questions and their answers from content. Top N specifies the number to extract',
        6, true);
insert into adi_workflow_component(uuid, name, title, remark, display_order, is_enable)
values (replace(gen_random_uuid()::text, '-', ''), 'Tongyiwanx', 'Wanx Image Generation', 'Call text-to-image model to generate images', 12, true);
insert into adi_workflow_component(uuid, name, title, remark, display_order, is_enable)
values (replace(gen_random_uuid()::text, '-', ''), 'HumanFeedback', 'Human Feedback',
        'Pause the running workflow and wait for user input, then continue executing the subsequent workflow after user input', 10, true);
insert into adi_workflow_component(uuid, name, title, remark, display_order, is_enable)
values (replace(gen_random_uuid()::text, '-', ''), 'MailSend', 'Email Send', 'Send email to a specified address', 10, true);
insert into adi_workflow_component(uuid, name, title, remark, display_order, is_enable)
values (replace(gen_random_uuid()::text, '-', ''), 'HttpRequest', 'HTTP Request',
        'Send requests via HTTP protocol. Outputs from other components can be used as parameters, and constants can also be set as parameters.', 10, true);

-- Demo workflow: Translate to Chinese
insert into adi_workflow(uuid, title, user_id, is_public, is_enable)
values ('c40cfc1792264130b1c1f82d1448648f', 'Translate to Chinese', 1, true, true);

-- If the input variable name is not defined, it defaults to "input"
-- If the output variable name is not defined, it defaults to "output"
INSERT INTO adi_workflow_node (uuid, workflow_id, workflow_component_id, user_id, title, input_config, node_config,
                               position_x, position_y)
VALUES ('6cbc919774aa4e779d97e3dd9c836e16', 1, 1, 0, 'Start', '{
  "ref_inputs": [],
  "user_inputs": [
    {
      "name": "var_input",
      "type": 1,
      "uuid": "aed4841dfabf4e08a9cfaea98f143c08",
      "title": "Content to translate",
      "required": true
    }
  ]
}', '{}', -402.11898718515704, 325.1042741060348);
INSERT INTO adi_workflow_node (uuid, workflow_id, workflow_component_id, user_id, title, input_config, node_config,
                               position_x, position_y)
VALUES ('7bb3d7b63d2c4773aafde36fca9b67e9', 1, 3, 0, 'Translate', '{
  "ref_inputs": [
    {
      "name": "var_rising",
      "node_uuid": "6cbc919774aa4e779d97e3dd9c836e16",
      "node_param_name": "var_input"
    }
  ],
  "user_inputs": []
}', '{
  "prompt": "Translate the following content into Chinese. Output only the translation result without any explanation, prefix, or suffix:\n{var_rising}",
  "model_name": "deepseek-v4-flash"
}', -105.91822303004142, 328.82173298096063);
INSERT INTO adi_workflow_node (uuid, workflow_id, workflow_component_id, user_id, title, input_config, node_config,
                               position_x, position_y)
VALUES ('7a71f5860723457ba5815a71cb21f4d5', 1, 9, 0, 'Classify Content', '{
  "ref_inputs": [],
  "user_inputs": []
}', '{
  "categories": [
    {
      "category_name": "Negative",
      "category_uuid": "f325d5e3545a42a184648b88eaaa9f3a",
      "target_node_uuid": "4892a70af36b498e89ae461e9d9a9525"
    },
    {
      "category_name": "Positive",
      "category_uuid": "caa11c60ac86409a9c0284ef5ea6a284",
      "target_node_uuid": "e87c362ff13f489dad9568f94e8219ca"
    },
    {
      "category_name": "Other",
      "category_uuid": "ba542068bcb642f6a9287bcd0628af50",
      "target_node_uuid": "444bdaf7e35c4bb08aba35cfe42d7ee5"
    }
  ],
  "model_name": "deepseek-v4-flash"
}', 178.36431809503244, 256.2306178307531);
INSERT INTO adi_workflow_node (uuid, workflow_id, workflow_component_id, user_id, title, input_config, node_config,
                               position_x, position_y)
VALUES ('4892a70af36b498e89ae461e9d9a9525', 1, 8, 0, 'Conditional Branch', '{
  "ref_inputs": [],
  "user_inputs": []
}', '{
  "cases": [
    {
      "uuid": "cac3c57446584c2b8f809daa51091ebc",
      "operator": "or",
      "conditions": [
        {
          "uuid": "1ac3c57446584c2b8f809daa51091eb1",
          "value": "sad",
          "operator": "contains",
          "node_uuid": "7bb3d7b63d2c4773aafde36fca9b67e9",
          "node_param_name": "output"
        },
        {
          "uuid": "7113d7b63d2c4773aafde36fca9b6119",
          "value": "pain",
          "operator": "contains",
          "node_uuid": "7bb3d7b63d2c4773aafde36fca9b67e9",
          "node_param_name": "output"
        }
      ],
      "target_node_uuid": "7ac3c57446584c2b8f809daa51091eb0"
    },
    {
      "uuid": "ccb59926da754b7a8938eb3f400920ac",
      "operator": "and",
      "conditions": [
        {
          "uuid": "7111d7b63d2c4773aafde36fca9b1119",
          "value": "happy",
          "operator": "not contains",
          "node_uuid": "7bb3d7b63d2c4773aafde36fca9b67e9",
          "node_param_name": "output"
        }
      ],
      "target_node_uuid": "f2b59926da754b7a8938eb3f400920ac"
    }
  ],
  "default_target_node_uuid": "444bdaf7e35c4bb08aba35cfe42d7ee5"
}', 494.20826320047456, 21.11177315139355);
INSERT INTO adi_workflow_node (uuid, workflow_id, workflow_component_id, user_id, title, input_config, node_config,
                               position_x, position_y)
VALUES ('f2b59926da754b7a8938eb3f400920ac', 1, 3, 0, 'Give 1 Antonym', '{
  "ref_inputs": [
    {
      "name": "var_feel",
      "node_uuid": "7bb3d7b63d2c4773aafde36fca9b67e9",
      "node_param_name": "output"
    }
  ],
  "user_inputs": []
}', '{
  "prompt": "Give one antonym of the following content. Output only the antonym itself without any explanation, prefix, or suffix:\n{var_feel}",
  "model_name": "deepseek-v4-flash"
}', 891.5056599104687, 142.47976951620672);
INSERT INTO adi_workflow_node (uuid, workflow_id, workflow_component_id, user_id, title, input_config, node_config,
                               position_x, position_y)
VALUES ('7ac3c57446584c2b8f809daa51091eb0', 1, 3, 0, 'Give 10 Antonyms', '{
  "ref_inputs": [
    {
      "name": "var_fine",
      "node_uuid": "7bb3d7b63d2c4773aafde36fca9b67e9",
      "node_param_name": "output"
    }
  ],
  "user_inputs": []
}', '{
  "prompt": "Give 10 antonyms of the following content: {var_fine},\\n##Note\\nList the antonyms directly, separated by commas, with no extra words",
  "model_name": "deepseek-v4-flash"
}', 844.9517549754598, -7.5875938393296565);
INSERT INTO adi_workflow_node (uuid, workflow_id, workflow_component_id, user_id, title, input_config, node_config,
                               position_x, position_y)
VALUES ('1deb0f2df58c49768a3a5e59f825814d', 1, 10, 0, 'Content Template', '{
  "ref_inputs": [
    {
      "name": "var_among",
      "node_uuid": "6cbc919774aa4e77app9d97e3dd9c836e16",
      "node_param_name": "var_input"
    },
    {
      "name": "var_taught",
      "node_uuid": "7ac3c57446584c2b8f809daa51091eb0",
      "node_param_name": "output"
    }
  ],
  "user_inputs": []
}', '{
  "template": "Translation result: {var_among}, 10 antonyms: {var_taught}"
}', 1190.5986141955661, 70.7027457958036);
INSERT INTO adi_workflow_node (uuid, workflow_id, workflow_component_id, user_id, title, input_config, node_config,
                               position_x, position_y)
VALUES ('e87c362ff13f489dad9568f94e8219ca', 1, 3, 0, 'Give 1 Synonym', '{
  "ref_inputs": [
    {
      "name": "var_muscle",
      "node_uuid": "7bb3d7b63d2c4773aafde36fca9b67e9",
      "node_param_name": "output"
    }
  ],
  "user_inputs": []
}', '{
  "prompt": "Give one synonym of the following content: {var_muscle}",
  "model_name": "deepseek-v4-flash"
}', 575.9071882206999, 331.8698354997035);
INSERT INTO adi_workflow_node (uuid, workflow_id, workflow_component_id, user_id, title, input_config, node_config,
                               position_x, position_y)
VALUES ('444bdaf7e35c4bb08aba35cfe42d7ee5', 1, 2, 0, 'End', '{
  "ref_inputs": [
    {
      "name": "var_again",
      "node_uuid": "6cbc919774aa4e779d97e3dd9c836e16",
      "node_param_name": "var_input"
    },
    {
      "name": "var_soon",
      "node_uuid": "7bb3d7b63d2c4773aafde36fca9b67e9",
      "node_param_name": "output"
    }
  ],
  "user_inputs": []
}', '{
  "result": "User input: {var_again},\nClassification result (using default input, i.e. upstream node output): {input},\nTranslation result (using explicitly referenced variable): {var_soon}"
}', 1501.1327431135007, 486.4987875032814);

INSERT INTO adi_workflow_edge (uuid, workflow_id, source_node_uuid, source_handle, target_node_uuid)
VALUES ('e4f31f7b94f948339fd8a1a4800f665b', 1, '6cbc919774aa4e779d97e3dd9c836e16', '',
        '7bb3d7b63d2c4773aafde36fca9b67e9');
INSERT INTO adi_workflow_edge (uuid, workflow_id, source_node_uuid, source_handle, target_node_uuid)
VALUES ('44cb1e8fa83c4a27abd3c2932b620b32', 1, '7bb3d7b63d2c4773aafde36fca9b67e9', '',
        '7a71f5860723457ba5815a71cb21f4d5');
INSERT INTO adi_workflow_edge (uuid, workflow_id, source_node_uuid, source_handle, target_node_uuid)
VALUES ('f71209d95e824137b7016a9dc4c3d6f4', 1, '7a71f5860723457ba5815a71cb21f4d5', 'ba542068bcb642f6a9287bcd0628af50',
        '444bdaf7e35c4bb08aba35cfe42d7ee5');
INSERT INTO adi_workflow_edge (uuid, workflow_id, source_node_uuid, source_handle, target_node_uuid)
VALUES ('2b724ee9e7334e70b2d762859afad578', 1, '4892a70af36b498e89ae461e9d9a9525', 'default_handle',
        '444bdaf7e35c4bb08aba35cfe42d7ee5');
INSERT INTO adi_workflow_edge (uuid, workflow_id, source_node_uuid, source_handle, target_node_uuid)
VALUES ('3699e97130634a409e533f685bb291d8', 1, 'e87c362ff13f489dad9568f94e8219ca', '',
        '444bdaf7e35c4bb08aba35cfe42d7ee5');
INSERT INTO adi_workflow_edge (uuid, workflow_id, source_node_uuid, source_handle, target_node_uuid)
VALUES ('2fffb3c242254fe080a965dfe6adfc39', 1, 'f2b59926da754b7a8938eb3f400920ac', '',
        '444bdaf7e35c4bb08aba35cfe42d7ee5');
INSERT INTO adi_workflow_edge (uuid, workflow_id, source_node_uuid, source_handle, target_node_uuid)
VALUES ('bdd6b9530a284e13a083e8aba5bf3b19', 1, '7ac3c57446584c2b8f809daa51091eb0', '',
        '1deb0f2df58c49768a3a5e59f825814d');
INSERT INTO adi_workflow_edge (uuid, workflow_id, source_node_uuid, source_handle, target_node_uuid)
VALUES ('4bec71f5af29429b87f3928a9a20e3d8', 1, '1deb0f2df58c49768a3a5e59f825814d', '',
        '444bdaf7e35c4bb08aba35cfe42d7ee5');
INSERT INTO adi_workflow_edge (uuid, workflow_id, source_node_uuid, source_handle, target_node_uuid)
VALUES ('2e008c4200744f03a4ce0fba352d3d6c', 1, '7a71f5860723457ba5815a71cb21f4d5', 'f325d5e3545a42a184648b88eaaa9f3a',
        '4892a70af36b498e89ae461e9d9a9525');
INSERT INTO adi_workflow_edge (uuid, workflow_id, source_node_uuid, source_handle, target_node_uuid)
VALUES ('ecada0274c38429c99c6bce200d1d132', 1, '7a71f5860723457ba5815a71cb21f4d5', 'caa11c60ac86409a9c0284ef5ea6a284',
        'e87c362ff13f489dad9568f94e8219ca');
INSERT INTO adi_workflow_edge (uuid, workflow_id, source_node_uuid, source_handle, target_node_uuid)
VALUES ('9555c30dcc35410582029b1310321c63', 1, '4892a70af36b498e89ae461e9d9a9525', 'cac3c57446584c2b8f809daa51091ebc',
        '7ac3c57446584c2b8f809daa51091eb0');
INSERT INTO adi_workflow_edge (uuid, workflow_id, source_node_uuid, source_handle, target_node_uuid)
VALUES ('768794ce3d9d4484b2f3d1fcf7cf29ad', 1, '4892a70af36b498e89ae461e9d9a9525', 'ccb59926da754b7a8938eb3f400920ac',
        'f2b59926da754b7a8938eb3f400920ac');

-- MCP servers
-- Amap (Gaode Maps), no installation required, ready to use
-- The preset_params here contain the Amap API key. preset_params represent system-wide shared default configuration, so users do not need to obtain a key from the Amap website to use this MCP
insert into adi_mcp (uuid, title, transport_type, sse_url, sse_timeout, install_type, preset_params, website, remark,
                     is_enable)
values (replace(gen_random_uuid()::text, '-', ''), 'Amap (Gaode Maps)', 'sse', 'https://mcp.amap.com/sse', 30, 'remote',
        '[
          {
            "name": "key",
            "title": "Amap Service API Key",
            "value": "Fill in your Amap API key here (shared by system)",
            "require_encrypt": true,
            "encrypted": false
          }
        ]', 'https://lbs.amap.com/api/mcp-server/summary',
        '## Product Features

* Easy to use: Suitable for regular users via MCP (SSE) mode. No local service deployment needed -- simply configure via URL.
* Automatic updates: We continuously iterate and update without any additional user action required.
* Easier for LLM understanding: We have performed semantic conversion on the original JSON results, making them easier for LLMs to understand.
* Zero maintenance cost: Fully managed cloud service architecture. Users do not need to worry about server maintenance, resource scaling, or other infrastructure concerns.
* Protocol compatibility: Supports SSE long connections, adapting to different business scenario requirements.

## Capabilities

* Generate personalized maps
* Navigate to destination
* Ride-hailing
* Geocoding
* Reverse geocoding
* IP-based location
* Weather query
* Cycling route planning
* Walking route planning
* Driving route planning
* Public transit route planning
* Distance measurement
* Keyword search
* Nearby search
* Detail search',
        true);
-- Brave Search, installed locally via npx
-- The customized_param_definitions here specify the parameters each user needs to fill in (i.e. the key obtained from Brave Search), meaning every user must provide this key before using this MCP service for initialization and invocation
insert into adi_mcp (uuid, title, transport_type, stdio_command, stdio_arg, install_type, customized_param_definitions,
                     website,
                     remark,
                     is_enable)
values (replace(gen_random_uuid()::text, '-', ''), 'Brave Search', 'stdio', 'npx',
        '-y @modelcontextprotocol/server-brave-search', 'local',
        '[
          {
            "name": "BRAVE_API_KEY",
            "title": "Brave Search API key",
            "require_encrypt": true
          }
        ]', 'https://github.com/modelcontextprotocol/servers-archived/tree/main/src/brave-search',
        '# Brave Search MCP Server

An MCP server implementation that integrates the Brave Search API, providing both web and local search capabilities.

## Features

- **Web Search**: General queries, news, articles, with pagination and freshness controls
- **Local Search**: Find businesses, restaurants, and services with detailed information
- **Flexible Filtering**: Control result types, safety levels, and content freshness
- **Smart Fallbacks**: Local search automatically falls back to web when no results are found

## Tools

- **brave_web_search**

  - Execute web searches with pagination and filtering
  - Inputs:
    - `query` (string): Search terms
    - `count` (number, optional): Results per page (max 20)
    - `offset` (number, optional): Pagination offset (max 9)

- **brave_local_search**
  - Search for local businesses and services
  - Inputs:
    - `query` (string): Local search terms
    - `count` (number, optional): Number of results (max 20)
  - Automatically falls back to web search if no local results found

## Configuration

### Getting an API Key

1. Sign up for a [Brave Search API account](https://brave.com/search/api/)
2. Choose a plan (Free tier available with 2,000 queries/month)
3. Generate your API key [from the developer dashboard](https://api-dashboard.search.brave.com/app/keys)',
        true);

insert into adi_user_mcp (uuid, user_id, mcp_id, mcp_customized_params, is_enable)
values (replace(gen_random_uuid()::text, '-', ''), 1, 2, '[
  {
    "name": "BRAVE_API_KEY",
    "value": "Fill in your Brave Search API key here",
    "encrypted": false
  }
]', true);
