-- noinspection SqlNoDataSourceInspectionForFile

-- noinspection SqlNoDataSourceInspectionForFile

-- 安装pgvector扩展（https://github.com/pgvector/pgvector）
-- 安装Apache AGE扩展（https://github.com/apache/age）
-- CREATE EXTENSION IF NOT EXISTS vector;
-- CREATE EXTENSION IF NOT EXISTS age;

SET client_encoding = 'UTF8';
CREATE SCHEMA public;

-- ============================================================
-- Common: update_time trigger function
-- ============================================================
CREATE OR REPLACE FUNCTION update_modified_column()
    RETURNS TRIGGER AS
$$
BEGIN
    NEW.update_time = CURRENT_TIMESTAMP;
    RETURN NEW;
END;
$$ language 'plpgsql';

-- ============================================================
-- Drawing: AI image generation and gallery
-- ============================================================

-- 原表名: adi_ai_image
CREATE TABLE adi_draw
(
    id                    bigserial primary key,
    user_id               bigint        default 0                 not null,
    uuid                  varchar(32)   default ''                not null,
    ai_model_id           bigint        default 0                 not null,
    ai_model_name         varchar(45)   default ''                not null,
    prompt                varchar(1024) default ''                not null,
    negative_prompt       varchar(1024) default ''                not null,
    generate_size         varchar(20)   default ''                not null,
    generate_quality      varchar(20)   default ''                not null,
    generate_number       integer       default 1                 not null,
    generate_seed         integer       default -1                not null,
    dynamic_params        jsonb         default '{}'              not null,
    original_image        varchar(1000) default ''                not null,
    mask_image            varchar(1000) default ''                not null,
    resp_images_path      varchar(2048) default ''                not null,
    generated_images      varchar(2048) default ''                not null,
    interacting_method    smallint      default 1                 not null,
    process_status        smallint      default 1                 not null,
    process_status_remark varchar(250)  default ''                not null,
    is_public             boolean       default false             not null,
    with_watermark        boolean       default false             not null,
    star_count            int           default 0                 not null,
    create_time           timestamp     default CURRENT_TIMESTAMP not null,
    update_time           timestamp     default CURRENT_TIMESTAMP not null,
    is_deleted            boolean       default false             not null,
    CONSTRAINT adi_draw_generate_number_check CHECK (((generate_number >= 1) AND (generate_number <= 10))),
    CONSTRAINT adi_draw_process_status_check CHECK ((process_status = ANY (ARRAY [1, 2, 3]))),
    CONSTRAINT adi_draw_user_id_check CHECK ((user_id >= 0))
);
ALTER TABLE ONLY adi_draw
    ADD CONSTRAINT udx_uuid UNIQUE (uuid);
COMMENT ON TABLE adi_draw IS '绘图任务 | Image generation task';
COMMENT ON COLUMN adi_draw.user_id IS '用户ID | User ID';
COMMENT ON COLUMN adi_draw.uuid IS '绘图任务的UUID | UUID of the request of generated images';
COMMENT ON COLUMN adi_draw.ai_model_name IS '图像模型名称 | Image model name';
COMMENT ON COLUMN adi_draw.prompt IS '生成图片的提示词 | The prompt for generating images';
COMMENT ON COLUMN adi_draw.generate_size IS '生成图片的尺寸 | Image generation size';
COMMENT ON COLUMN adi_draw.generate_quality IS '生成图片的质量 | Image generation quality';
COMMENT ON COLUMN adi_draw.generate_number IS '生成图片的数量，必须在1到10之间，默认为1 | The number of images to generate. Must be between 1 and 10. Defaults to 1.';
COMMENT ON COLUMN adi_draw.generate_seed IS '生成图片的随机种子，如果希望生成内容保持相对稳定，请使用相同的seed参数值 | The random seed of the generated image. If you want the generated content to remain relatively stable, use the same seed parameter value';
COMMENT ON COLUMN adi_draw.original_image IS '原始图片的UUID，交互方式必须为2或3 | The UUID of the original image, interacting method must be 2 or 3';
COMMENT ON COLUMN adi_draw.mask_image IS '遮罩图片的UUID，交互方式必须为2 | The UUID of the mask image, interacting method must be 2';
COMMENT ON COLUMN adi_draw.resp_images_path IS '从OpenAI响应生成图片的URL，逗号隔开 | The URL of the generated images from OpenAI response, separated by commas';
COMMENT ON COLUMN adi_draw.generated_images IS '生成的多张图片文件UUID，逗号隔开 | The UUID of the generated images, separated by commas';
COMMENT ON COLUMN adi_draw.interacting_method IS '交互方式：1：文本生成图片；2：图片编辑；3：图片生成图片；4：背景生成；5：扩大图片；6：风格转化 | Image generation task type: 1: Text to Image; 2: Image Editing; 3: Image to Image; 4: Background Generation; 5: Style Transfer';
COMMENT ON COLUMN adi_draw.process_status IS '任务执行状态，1：进行中，2：失败，3：成功 | Task execution status, 1: In progress, 2: Failed, 3: Success';
COMMENT ON COLUMN adi_draw.process_status_remark IS '生成图片状态备注 | Image generation status remark';
COMMENT ON COLUMN adi_draw.is_public IS '是否公开 | Is public';
COMMENT ON COLUMN adi_draw.with_watermark IS '是否带水印 | With watermark';
COMMENT ON COLUMN adi_draw.star_count IS '点赞数 | Number of Likes';
COMMENT ON COLUMN adi_draw.create_time IS '记录创建的时间戳 | Timestamp of record creation';
COMMENT ON COLUMN adi_draw.update_time IS '记录最后更新的时间戳，自动更新 | Timestamp of record last update, automatically updated on each update';
COMMENT ON COLUMN adi_draw.is_deleted IS '记录是否被删除的标志（0：未删除，1：已删除） | Flag indicating whether the record is deleted (0: not deleted, 1: deleted)';

CREATE TRIGGER trigger_draw_update_time
    BEFORE UPDATE
    ON adi_draw
    FOR EACH ROW
EXECUTE PROCEDURE update_modified_column();

CREATE TABLE adi_draw_star
(
    id          bigserial primary key,
    user_id     bigint    default 0                 not null,
    draw_id     bigint    default 0                 not null,
    create_time timestamp default CURRENT_TIMESTAMP not null,
    update_time timestamp default CURRENT_TIMESTAMP not null,
    is_deleted  boolean   default false             not null
);
CREATE TRIGGER trigger_draw_star_update_time
    BEFORE UPDATE
    ON adi_draw_star
    FOR EACH ROW
EXECUTE PROCEDURE update_modified_column();

CREATE TABLE adi_draw_comment
(
    id          bigserial primary key,
    uuid        varchar(32) default ''                not null,
    user_id     bigint      default 0                 not null,
    draw_id     bigint      default 0                 not null,
    remark      text        default ''                not null,
    create_time timestamp   default CURRENT_TIMESTAMP not null,
    update_time timestamp   default CURRENT_TIMESTAMP not null,
    is_deleted  boolean     default false             not null
);
CREATE TRIGGER trigger_draw_comment_update_time
    BEFORE UPDATE
    ON adi_draw_comment
    FOR EACH ROW
EXECUTE PROCEDURE update_modified_column();

-- ============================================================
-- Model Platform & AI Model: model provider and model configuration
-- ============================================================

-- 旧版本（3.15.0及以下）中本表 adi_model_platform 的数据位于 adi_sys_config 中
-- 需要手动将 adi_sys_config 中对应的模型平台配置项移动到 adi_model_platform 表中
-- 需要迁移的配置为：deepseek_setting、openai_setting、dashscope_setting、ollama_setting、siliconflow_setting
CREATE TABLE adi_model_platform
(
    id                       bigserial primary key,
    name                     varchar(45)   default ''                not null,
    title                    varchar(45)   default ''                not null,
    base_url                 varchar(250)  default ''                not null,
    api_key                  varchar(100)  default ''                not null,
    secret_key               varchar(100)  default ''                not null,
    remark                   varchar(1000) default ''                not null,
    is_proxy_enable          boolean       default false             not null,
    is_openai_api_compatible boolean       default false             not null,
    create_time              timestamp     default CURRENT_TIMESTAMP not null,
    update_time              timestamp     default CURRENT_TIMESTAMP not null,
    is_deleted               boolean       default false             not null
);

COMMENT ON TABLE adi_model_platform IS '模型平台表（模型供应商表） | Model platform (model provider)';
COMMENT ON COLUMN adi_model_platform.name IS '模型平台名称，如openai, dashscope, ollama | Model provider name, e.g., openai, dashscope, ollama';
COMMENT ON COLUMN adi_model_platform.title IS '模型平台标题，可读性更高的名称，如: OpenAI，DeepSeek深度求索 | Model provider title, a more readable name, e.g., OpenAI, DeepSeek';
COMMENT ON COLUMN adi_model_platform.base_url IS '模型平台的API请求地址 | API base URL of the model provider';
COMMENT ON COLUMN adi_model_platform.api_key IS '模型平台的API Key | API Key of the model provider';
COMMENT ON COLUMN adi_model_platform.secret_key IS '已废弃，仅千帆平台使用，千帆已停用 | Deprecated, only used by Qianfan which is no longer supported';
COMMENT ON COLUMN adi_model_platform.is_proxy_enable IS '是否通过代理访问模型平台的API | Whether to access the model provider API through a proxy';
COMMENT ON COLUMN adi_model_platform.is_openai_api_compatible IS '是否兼容OpenAI的API，如果是，则可以使用OpenAI的API请求格式 | Whether it is compatible with OpenAI API, if true, OpenAI API request format can be used';
COMMENT ON COLUMN adi_model_platform.remark IS '备注 | Additional remarks about the model provider';

CREATE TRIGGER trigger_model_platform_update_time
    BEFORE UPDATE
    ON adi_model_platform
    FOR EACH ROW
EXECUTE PROCEDURE update_modified_column();

CREATE TABLE adi_ai_model
(
    id                    bigserial primary key,
    name                  varchar(45)   default ''                not null,
    title                 varchar(45)   default ''                not null,
    type                  varchar(45)   default 'text'            not null,
    setting               varchar(500)  default ''                not null,
    remark                varchar(1000) default '',
    platform              varchar(45)   default ''                not null,
    context_window        int           default 0                 not null,
    max_input_tokens      int           default 0                 not null,
    max_output_tokens     int           default 0                 not null,
    input_types           varchar(100)  default 'text'            not null,
    properties            jsonb         default '{}'              not null,
    response_format_types varchar(200)  default 'text'            not null,
    is_support_web_search boolean       default false             not null,
    is_reasoner           boolean       default false             not null,
    is_thinking_closable  boolean       default false             not null,
    is_free               boolean       default false             not null,
    is_enable             boolean       default false             not null,
    create_time           timestamp     default CURRENT_TIMESTAMP not null,
    update_time           timestamp     default CURRENT_TIMESTAMP not null,
    is_deleted            boolean       default false             not null
);

COMMENT ON TABLE adi_ai_model IS 'AI模型 | AI model';
COMMENT ON COLUMN adi_ai_model.type IS '模型类型, e.g., text, image, vision, embedding, rerank, asr, tts, multimodality(GPT-4o) | Model type, e.g., text, image, vision, embedding, rerank, multimodality(GPT-4o)';
COMMENT ON COLUMN adi_ai_model.name IS '模型名称，传到接口中请求响应的参数名，需跟模型提供方指定的模型名称一模一样 | Model name, the parameter name passed to the interface for requesting a response, must be exactly the same as the model name specified by the model provider';
COMMENT ON COLUMN adi_ai_model.title IS '模型标题，可读性更高的名称，如: openai-gpt3 | Model title, a more readable name, e.g., openai-gpt3';
COMMENT ON COLUMN adi_ai_model.setting IS 'json format, e.g., {voice_for_group1: "v1", voice_for_group2: "v2"}';
COMMENT ON COLUMN adi_ai_model.properties IS 'e.g., { "dimension": 1536 } for embedding model,{"voices":["v1","v2","v3"]} for tts model';
COMMENT ON COLUMN adi_ai_model.remark IS '备注 | Additional remarks about the AI model';
COMMENT ON COLUMN adi_ai_model.platform IS '平台，对应了 adi_model_platform.name | Model platform (as model provider): openai, dashscope, ollama';
COMMENT ON COLUMN adi_ai_model.context_window IS '上下文窗口 | LLM context window';
COMMENT ON COLUMN adi_ai_model.input_types IS '输入类型 | Input types: text, image, audio, video';
COMMENT ON COLUMN adi_ai_model.response_format_types IS '回复格式: text, json_object, json_schema | Response format: text, json_object, json_schema';
COMMENT ON COLUMN adi_ai_model.is_support_web_search IS '是否支持网络搜索 | Whether web search is supported';
COMMENT ON COLUMN adi_ai_model.is_reasoner IS 'true: 推理模型如deepseek-r1, false: 非推理模型如deepseek-v3 | true: Reasoning model, false: Non-reasoning model';
COMMENT ON COLUMN adi_ai_model.is_thinking_closable IS '思考过程是否可以关闭，Qwen3可以开启或关闭思考过程，而deepseek-r1无法关闭 | Whether the thinking process can be closed, Qwen3 can enable or disable the thinking process, while deepseek-r1 cannot disable it';
COMMENT ON COLUMN adi_ai_model.is_enable IS '是否启用 | True: Normal usage, false: Not available';
COMMENT ON COLUMN adi_ai_model.is_free IS '是否免费 | Is free, true: free, false: paid';
COMMENT ON COLUMN adi_ai_model.create_time IS '创建时间 | Timestamp of record creation';
COMMENT ON COLUMN adi_ai_model.update_time IS '更新时间 | Timestamp of record last update, automatically updated on each update';

CREATE TRIGGER trigger_ai_model_update_time
    BEFORE UPDATE
    ON adi_ai_model
    FOR EACH ROW
EXECUTE PROCEDURE update_modified_column();

-- ============================================================
-- Conversation: chat sessions, presets, and messages
-- ============================================================

CREATE TABLE adi_conversation_preset
(
    id                bigserial primary key,
    uuid              varchar(32)   default ''                not null,
    title             varchar(45)   default ''                not null,
    remark            varchar(1000) default ''                not null,
    ai_system_message varchar(1000) default ''                not null,
    create_time       timestamp     default CURRENT_TIMESTAMP not null,
    update_time       timestamp     default CURRENT_TIMESTAMP not null,
    is_deleted        boolean       default false             not null
);
COMMENT ON TABLE adi_conversation_preset IS '预设会话(角色)表 | Preset conversation (role) table';
COMMENT ON COLUMN adi_conversation_preset.title IS '标题 | Title';
COMMENT ON COLUMN adi_conversation_preset.remark IS '描述 | Description';
COMMENT ON COLUMN adi_conversation_preset.ai_system_message IS '提供给LLM的系统信息 | System message for LLM';

create trigger trigger_conversation_preset
    before update
    on adi_conversation_preset
    for each row
execute procedure update_modified_column();

CREATE TABLE adi_conversation
(
    id                        bigserial primary key,
    user_id                   bigint        default 0                 not null,
    uuid                      varchar(32)   default ''                not null,
    title                     varchar(45)   default ''                not null,
    remark                    varchar(500)  default ''                not null,
    tokens                    integer       default 0                 not null,
    ai_system_message         varchar(1000) default ''                not null,
    understand_context_enable boolean       default false             not null,
    llm_temperature           numeric(2, 1) default 0.7               not null,
    mcp_ids                   varchar(1000) default ''                not null,
    kb_ids                    varchar(1000) default ''                not null,
    answer_content_type       smallint      default 1                 not null,
    is_autoplay_answer        boolean       default true              not null,
    is_enable_thinking        boolean       default false             not null,
    is_enable_web_search      boolean       default false             not null,
    audio_config              jsonb         default '{}'              not null,
    create_time               timestamp     default CURRENT_TIMESTAMP not null,
    update_time               timestamp     default CURRENT_TIMESTAMP not null,
    is_deleted                boolean       default false             not null
);

COMMENT ON TABLE adi_conversation IS '用户会话(角色)表 | User conversation (role) table';
COMMENT ON COLUMN adi_conversation.user_id IS '用户ID | User ID';
COMMENT ON COLUMN adi_conversation.title IS '标题，如：狄仁杰 | Title, e.g., Sherlock Holmes';
COMMENT ON COLUMN adi_conversation.remark IS '备注，如：断案如神，手下能人众多 | Remark, e.g., Brilliant detective with keen observation skills';
COMMENT ON COLUMN adi_conversation.ai_system_message IS '角色设定内容，如：你是唐朝的狄仁杰，破了很多大案、疑案 | Role setting content, e.g., You are Sherlock Holmes, a brilliant detective known for your keen observation skills';
COMMENT ON COLUMN adi_conversation.llm_temperature IS 'LLM响应的创造性/随机性 | LLM response creativity/randomness';
COMMENT ON COLUMN adi_conversation.mcp_ids IS '启用的MCP服务id,以逗号隔开 | Enabled MCP service IDs, comma-separated';
COMMENT ON COLUMN adi_conversation.kb_ids IS '关联使用的知识库id列表,以逗号隔开 | Associated knowledge base IDs, comma-separated';
COMMENT ON COLUMN adi_conversation.answer_content_type IS '设置响应内容类型：1：自动（跟随用户的输入类型，如果用户输入是音频，则响应内容也同样是音频，如果用户输入是文本，则响应内容显示文本），2：文本，3：音频 | Response content display type: 1: Auto (if user input is audio, response content is also audio; if user input is text, response content displays text), 2: Text, 3: Audio';
COMMENT ON COLUMN adi_conversation.is_autoplay_answer IS '设置聊天时音频类型的响应内容是否自动播放，true: 自动播放，false: 不自动播放 | Whether audio-type response content automatically plays, true: Auto play, false: Do not auto play';
COMMENT ON COLUMN adi_conversation.is_enable_thinking IS '当前使用的模型如果是推理模式并且支持对思考过程的开关，则本字段生效 | Whether the current model supports reasoning mode and thinking process toggle, if so, this field takes effect';
COMMENT ON COLUMN adi_conversation.is_enable_web_search IS '是否启用web搜索 | Whether to enable web search';
COMMENT ON COLUMN adi_conversation.audio_config IS '音频配置，json格式存储，如 {"voice":{"param_name":"longyingda","model":"cosyvoice-v2","platform":"dashscope"}} | Audio configuration, stored in JSON format, e.g., {"voice":{"param_name":"longyingda","model":"cosyvoice-v2","platform":"dashscope"}}';


CREATE TRIGGER trigger_conv_update_time
    BEFORE UPDATE
    ON adi_conversation
    FOR EACH ROW
EXECUTE PROCEDURE update_modified_column();


CREATE TABLE adi_conversation_preset_rel
(
    id             bigserial primary key,
    uuid           varchar(32) default ''                not null,
    user_id        bigint      default 0                 not null,
    preset_conv_id bigint      default 0                 not null,
    user_conv_id   bigint      default 0                 not null,
    create_time    timestamp   default CURRENT_TIMESTAMP not null,
    update_time    timestamp   default CURRENT_TIMESTAMP not null,
    is_deleted     boolean     default false             not null
);

COMMENT ON TABLE adi_conversation_preset_rel IS '预设会话与用户会话关系表 | Preset conversation and user conversation relationship table';
COMMENT ON COLUMN adi_conversation_preset_rel.user_id IS '用户ID | User ID';
COMMENT ON COLUMN adi_conversation_preset_rel.preset_conv_id IS '预设会话ID | Preset conversation ID';
COMMENT ON COLUMN adi_conversation_preset_rel.user_conv_id IS '用户会话ID | User conversation ID';

create trigger trigger_conversation_preset_rel
    before update
    on adi_conversation_preset_rel
    for each row
execute procedure update_modified_column();

CREATE TABLE adi_conversation_message
(
    id                              bigserial primary key,
    parent_message_id               bigint        default 0                 not null,
    conversation_id                 bigint        default 0                 not null,
    conversation_uuid               varchar(32)   default ''                not null,
    content_type                    smallint      default 2                 not null,
    remark                          text          default ''                not null,
    processed_remark                text          default ''                not null,
    thinking_content                text          default ''                not null,
    audio_uuid                      varchar(32)   default ''                not null,
    audio_duration                  integer       default 0                 not null,
    uuid                            varchar(32)   default ''                not null,
    message_role                    integer       default 1                 not null,
    tokens                          integer       default 0                 not null,
    user_id                         bigint        default 0                 not null,
    ai_model_id                     bigint        default 0                 not null,
    understand_context_msg_pair_num integer       default 0                 not null,
    attachments                     varchar(1000) default ''                not null,
    create_time                     timestamp     default CURRENT_TIMESTAMP not null,
    update_time                     timestamp     default CURRENT_TIMESTAMP not null,
    is_ref_embedding                boolean       default false             not null,
    is_ref_graph                    boolean       default false             not null,
    is_ref_memory_embedding         boolean       default false             not null,
    is_deleted                      boolean       default false             not null
);

COMMENT ON TABLE adi_conversation_message IS '对话消息表 | Conversation message table';
COMMENT ON COLUMN adi_conversation_message.parent_message_id IS '父级消息id | Parent message ID';
COMMENT ON COLUMN adi_conversation_message.conversation_id IS '对话id | Conversation ID';
COMMENT ON COLUMN adi_conversation_message.conversation_uuid IS '对话的UUID | Conversation UUID';
COMMENT ON COLUMN adi_conversation_message.remark IS '原始的对话消息，如用户输入的问题，AI产生的回答';
COMMENT ON COLUMN adi_conversation_message.processed_remark IS '处理过的有效的对话消息，如 1.提供给LLM的内容：用户输入的问题+关联的知识库；2.显示在用户面前的答案：AI产生的回答经过合规校验及过滤、个性化调整后的内容';
COMMENT ON COLUMN adi_conversation_message.content_type IS '消息内容类型（跟conversation.answer_content_type对应），2：文本，3：音频 | Message content type, 2: Text, 3: Audio';
COMMENT ON COLUMN adi_conversation_message.uuid IS '唯一标识消息的UUID | Unique identifier for the message';
COMMENT ON COLUMN adi_conversation_message.audio_uuid IS '语音聊天时产生的音频文件uuid(对应adi_file.uuid) | UUID of the audio file generated during voice chat (corresponds to adi_file.uuid)';
COMMENT ON COLUMN adi_conversation_message.audio_duration IS '语音聊天时产生的音频文件时长(单位:秒) | Duration of the audio file generated during voice chat (in seconds)';
COMMENT ON COLUMN adi_conversation_message.message_role IS '产生该消息的角色：1: 用户, 2: 系统, 3: 助手 | Role that generated the message: 1: User, 2: System, 3: Assistant';
COMMENT ON COLUMN adi_conversation_message.tokens IS '消耗的token数量 | Number of tokens consumed';
COMMENT ON COLUMN adi_conversation_message.user_id IS '用户ID | User ID';
COMMENT ON COLUMN adi_conversation_message.ai_model_id IS '模型表的ID | adi_ai_model id';
COMMENT ON COLUMN adi_conversation_message.understand_context_msg_pair_num IS '上下文消息对数量 | Number of context message pairs';
COMMENT ON COLUMN adi_conversation_message.attachments IS '附件,存储格式: uuid,uuid | Attachments, stored as: uuid,uuid';
COMMENT ON COLUMN adi_conversation_message.is_ref_embedding IS '是否引用了向量库知识 | Whether embedding knowledge is referenced';
COMMENT ON COLUMN adi_conversation_message.is_ref_graph IS '是否引用了图库知识 | Whether graph knowledge is referenced';
COMMENT ON COLUMN adi_conversation_message.is_ref_memory_embedding IS '是否引用了记忆向量库 | Whether to reference memory vector library';

CREATE TRIGGER trigger_conv_message_update_time
    BEFORE UPDATE
    ON adi_conversation_message
    FOR EACH ROW
EXECUTE PROCEDURE update_modified_column();

create table adi_conversation_message_ref_embedding
(
    id           bigserial primary key,
    message_id   bigint        default 0  not null,
    embedding_id varchar(36)   default '' not null,
    score        numeric(3, 2) default 0  not null,
    user_id      bigint        default 0  not null
);

comment on table adi_conversation_message_ref_embedding is '会话消息-知识库的向量引用 | Conversation-Question Record-Knowledge Base Embedding Reference List';
comment on column adi_conversation_message_ref_embedding.message_id is '消息id | adi_conversation_message ID';
comment on column adi_conversation_message_ref_embedding.embedding_id is '根据消息从向量库中获取到的向量uuid | adi_knowledge_base_embedding UUID';
comment on column adi_conversation_message_ref_embedding.score is '评分 | Score';
comment on column adi_conversation_message_ref_embedding.user_id is '所属用户 | User ID';

create table adi_conversation_message_ref_graph
(
    id                     bigserial primary key,
    message_id             bigint default 0  not null,
    entities_from_question text   default '' not null,
    graph_from_store       text   default '' not null,
    user_id                bigint default 0  not null
);

comment on table adi_conversation_message_ref_graph is '会话消息-知识库的图谱引用记录 | Knowledge Base - Question Records - Graph References';
comment on column adi_conversation_message_ref_graph.message_id is '消息id | adi_conversation_message ID';
comment on column adi_conversation_message_ref_graph.entities_from_question is '从问题中解析出来的实体: vertexName1,vertexName2 | Graph parsed by LLM: vertexName1,vertexName2';
comment on column adi_conversation_message_ref_graph.graph_from_store is '根据消息从图数据库中查找到的图谱: {vertices:[{id:"111",name:"vertexName1"},{id:"222",name:"vertexName2"}],edges:[{id:"333",name:"edgeName1",start:"111",end:"222"}] | Graph retrieved from graph database: {vertices:[{id:"111",name:"vertexName1"},{id:"222",name:"vertexName2"}],edges:[{id:"333",name:"edgeName1",start:"111",end:"222"}]';
comment on column adi_conversation_message_ref_graph.user_id is '所属用户 | adi_user ID';

create table adi_conversation_message_ref_memory_embedding
(
    id           bigserial primary key,
    message_id   bigint        default 0  not null,
    embedding_id varchar(36)   default '' not null,
    score        numeric(3, 2) default 0  not null,
    user_id      bigint        default 0  not null
);
comment on table adi_conversation_message_ref_memory_embedding is '会话消息-知识库的记忆引用 | Conversation-Question Record-Memory References';
comment on column adi_conversation_message_ref_memory_embedding.message_id is '消息id | adi_conversation_message ID';
comment on column adi_conversation_message_ref_memory_embedding.embedding_id is '根据消息从记忆向量库中获取到的向量uuid | adi_conversation_memory_embedding UUID';
comment on column adi_conversation_message_ref_memory_embedding.score is '评分 | Score';
comment on column adi_conversation_message_ref_memory_embedding.user_id is '所属用户 | User ID';

-- ============================================================
-- File & Prompt: file storage and prompt templates
-- ============================================================

CREATE TABLE adi_file
(
    id               bigserial primary key,
    name             varchar(120) default ''                not null,
    uuid             varchar(32)  default ''                not null,
    ext              varchar(36)  default ''                not null,
    user_id          bigint       default 0                 not null,
    path             varchar(250) default ''                not null,
    storage_location int          default 1                 not null,
    ref_count        integer      default 0                 not null,
    create_time      timestamp    default CURRENT_TIMESTAMP not null,
    update_time      timestamp    default CURRENT_TIMESTAMP not null,
    is_deleted       boolean      default false             not null,
    sha256           varchar(64)  default ''                not null
);

COMMENT ON TABLE adi_file IS '文件 | File';
COMMENT ON COLUMN adi_file.name IS '文件名 | File name';
COMMENT ON COLUMN adi_file.uuid IS '文件的UUID | UUID of the file';
COMMENT ON COLUMN adi_file.ext IS '文件扩展名 | File extension';
COMMENT ON COLUMN adi_file.user_id IS '用户ID，0: 系统；其他: 用户 | User ID, 0: System; Other: User';
COMMENT ON COLUMN adi_file.path IS '文件路径或对象名称(OSS)，如https://*.png 或 123.png | File path or object name, e.g., httts://*.png or 123.png(name in OSS bucket)';
COMMENT ON COLUMN adi_file.storage_location IS '存储位置，1：本地存储，2：阿里云OSS | Storage Location: 1 - Local Storage, 2 - Alibaba Cloud OSS';
COMMENT ON COLUMN adi_file.ref_count IS '引用此文件的次数 | The number of references to this file';
COMMENT ON COLUMN adi_file.create_time IS '记录创建的时间戳 | Timestamp of record creation';
COMMENT ON COLUMN adi_file.update_time IS '记录最后更新的时间戳，自动更新 | Timestamp of record last update, automatically updated on each update';
COMMENT ON COLUMN adi_file.is_deleted IS '是否删除，0: 正常；1: 删除 | Deletion status, 0: Normal; 1: Deleted';
COMMENT ON COLUMN adi_file.sha256 IS '文件的哈希值 | Hash of the file';
CREATE TRIGGER trigger_file_update_time
    BEFORE UPDATE
    ON adi_file
    FOR EACH ROW
EXECUTE PROCEDURE update_modified_column();

CREATE TABLE adi_prompt
(
    id          bigserial primary key,
    user_id     bigint                 default 0                 not null,
    act         character varying(120) default ''                not null,
    prompt      text                                             not null,
    create_time timestamp              default CURRENT_TIMESTAMP not null,
    update_time timestamp              default CURRENT_TIMESTAMP not null,
    is_deleted  boolean                default false             not null
);

COMMENT ON TABLE adi_prompt IS '提示词';

COMMENT ON COLUMN adi_prompt.user_id IS '所属用户(0: system)';

COMMENT ON COLUMN adi_prompt.act IS '提示词标题';

COMMENT ON COLUMN adi_prompt.prompt IS '提示词内容';

COMMENT ON COLUMN adi_prompt.create_time IS 'Timestamp of record creation';

COMMENT ON COLUMN adi_prompt.update_time IS 'Timestamp of record last update, automatically updated on each update';

COMMENT ON COLUMN adi_prompt.is_deleted IS '0:未删除；1：已删除';

CREATE TRIGGER trigger_prompt_update_time
    BEFORE UPDATE
    ON adi_prompt
    FOR EACH ROW
EXECUTE PROCEDURE update_modified_column();

-- ============================================================
-- System Config & User: system configuration and user management
-- ============================================================

CREATE TABLE adi_sys_config
(
    id          bigserial primary key,
    name        character varying(100)  default ''             not null,
    value       character varying(1000) default ''             not null,
    create_time timestamp               default localtimestamp not null,
    update_time timestamp               default localtimestamp not null,
    is_deleted  boolean                 default false          not null
);

COMMENT ON TABLE adi_sys_config IS '系统配置表 | System configuration table';
COMMENT ON COLUMN adi_sys_config.name IS '配置项名称 | Configuration item name';
COMMENT ON COLUMN adi_sys_config.value IS '配置项值 | Configuration item value';
COMMENT ON COLUMN adi_sys_config.create_time IS '记录创建的时间戳 | Timestamp of record creation';
COMMENT ON COLUMN adi_sys_config.update_time IS '记录最后更新的时间戳，自动更新 | Timestamp of record last update, automatically updated on each update';
COMMENT ON COLUMN adi_sys_config.is_deleted IS '是否删除，0：未删除；1：已删除 | Deletion status, 0: Not deleted; 1: Deleted';

CREATE TRIGGER trigger_sys_config_update_time
    BEFORE UPDATE
    ON adi_sys_config
    FOR EACH ROW
EXECUTE PROCEDURE update_modified_column();

CREATE TABLE adi_user
(
    id                              bigserial primary key,
    name                            character varying(45)  default ''                not null,
    password                        character varying(120) default ''                not null,
    uuid                            character varying(32)  default ''                not null,
    email                           character varying(120) default ''                not null,
    active_time                     timestamp,
    user_status                     smallint               default '1'::smallint     not null,
    is_admin                        boolean                default false             not null,
    quota_by_token_daily            integer                default 0                 not null,
    quota_by_token_monthly          integer                default 0                 not null,
    quota_by_request_daily          integer                default 0                 not null,
    quota_by_request_monthly        integer                default 0                 not null,
    understand_context_enable       smallint               default '0'::smallint     not null,
    understand_context_msg_pair_num integer                default 3                 not null,
    quota_by_image_daily            integer                default 0                 not null,
    quota_by_image_monthly          integer                default 0                 not null,
    locale                          character varying(10)  default ''                not null,
    create_time                     timestamp              default CURRENT_TIMESTAMP not null,
    update_time                     timestamp              default CURRENT_TIMESTAMP not null,
    is_deleted                      boolean                default false             not null
);

COMMENT ON TABLE adi_user IS '用户表 | User table';
COMMENT ON COLUMN adi_user.name IS '用户名 | Username';
COMMENT ON COLUMN adi_user.password IS '密码 | Password';
COMMENT ON COLUMN adi_user.uuid IS '用户的UUID | UUID of the user';
COMMENT ON COLUMN adi_user.email IS '用户邮箱 | User email';
COMMENT ON COLUMN adi_user.active_time IS '激活时间 | Activation time';
COMMENT ON COLUMN adi_user.create_time IS '记录创建的时间戳 | Timestamp of record creation';
COMMENT ON COLUMN adi_user.update_time IS '记录最后更新的时间戳，自动更新 | Timestamp of record last update, automatically updated on each update';
COMMENT ON COLUMN adi_user.user_status IS '用户状态，1：待验证；2：正常；3：冻结 | User status, 1: Pending verification; 2: Active; 3: Frozen';
COMMENT ON COLUMN adi_user.is_admin IS '是否管理员，0：否；1：是 | Is admin, 0: No; 1: Yes';
COMMENT ON COLUMN adi_user.is_deleted IS '是否删除，0：未删除；1：已删除 | Deletion status, 0: Not deleted; 1: Deleted';
COMMENT ON COLUMN adi_user.quota_by_token_daily IS '每日token配额 | Daily token quota';
COMMENT ON COLUMN adi_user.quota_by_token_monthly IS '每月token配额 | Monthly token quota';
COMMENT ON COLUMN adi_user.quota_by_request_daily IS '每日请求配额 | Daily request quota';
COMMENT ON COLUMN adi_user.quota_by_request_monthly IS '每月请求配额 | Monthly request quota';
COMMENT ON COLUMN adi_user.understand_context_enable IS '上下文理解开关 | Context understanding switch';
COMMENT ON COLUMN adi_user.understand_context_msg_pair_num IS '上下文消息对数量 | Number of context message pairs';
COMMENT ON COLUMN adi_user.quota_by_image_daily IS '每日图片配额 | Daily image quota';
COMMENT ON COLUMN adi_user.quota_by_image_monthly IS '每月图片配额 | Monthly image quota';

CREATE TRIGGER trigger_user_update_time
    BEFORE UPDATE
    ON adi_user
    FOR EACH ROW
EXECUTE PROCEDURE update_modified_column();

CREATE TABLE adi_user_day_cost
(
    id            bigserial primary key,
    user_id       bigint    default 0                 not null,
    day           integer   default 0                 not null,
    tokens        integer   default 0                 not null,
    draw_times    integer   default 0                 not null,
    request_times integer   default 0                 not null,
    is_free       boolean   default false             not null,
    create_time   timestamp default CURRENT_TIMESTAMP not null,
    update_time   timestamp default CURRENT_TIMESTAMP not null,
    is_deleted    boolean   default false             not null
);

COMMENT ON TABLE adi_user_day_cost IS '用户每天消耗总量表 | User daily consumption table';
COMMENT ON COLUMN adi_user_day_cost.user_id IS '用户ID | User ID';
COMMENT ON COLUMN adi_user_day_cost.day IS '日期，用7位整数表示，如20230901 | Date, represented as a 7-digit integer, e.g., 20230901';
COMMENT ON COLUMN adi_user_day_cost.request_times IS '请求数量 | Number of requests';
COMMENT ON COLUMN adi_user_day_cost.tokens IS '消耗的token数量 | Number of tokens consumed';
COMMENT ON COLUMN adi_user_day_cost.is_free IS '是：免费额度(即该行统计的是免费模型消耗的额度)；否：收费额度(即该行统计的是收费模型消耗的额度) | Yes: Free quota (the row counts the consumption of free models); No: Paid quota (the row counts the consumption of paid models)';
COMMENT ON COLUMN adi_user_day_cost.create_time IS '记录创建的时间戳 | Timestamp of record creation';
COMMENT ON COLUMN adi_user_day_cost.update_time IS '记录最后更新的时间戳，自动更新 | Timestamp of record last update, automatically updated on each update';
COMMENT ON COLUMN adi_user_day_cost.draw_times IS '图片数量 | Number of images';

CREATE TRIGGER trigger_user_day_cost_update_time
    BEFORE UPDATE
    ON adi_user_day_cost
    FOR EACH ROW
EXECUTE PROCEDURE update_modified_column();

-- ============================================================
-- Knowledge Base: RAG with vector and graph search
-- ============================================================

create table adi_knowledge_base
(
    id                     bigserial primary key,
    uuid                   varchar(32)   default ''                not null,
    title                  varchar(250)  default ''                not null,
    remark                 text          default ''                not null,
    is_public              boolean       default false             not null,
    is_strict              boolean       default true              not null,
    ingest_max_overlap     int           default 0                 not null,
    ingest_model_name      varchar(45)   default ''                not null,
    ingest_model_id        bigint        default 0                 not null,
    ingest_token_estimator varchar(45)   default ''                not null,
    ingest_embedding_model varchar(45)   default ''                not null,
    retrieve_max_results   int           default 3                 not null,
    retrieve_min_score     numeric(2, 1) default 0.6               not null,
    query_llm_temperature  numeric(2, 1) default 0.7               not null,
    query_system_message   varchar(1000) default ''                not null,
    owner_id               bigint        default 0                 not null,
    owner_uuid             varchar(32)   default ''                not null,
    owner_name             varchar(45)   default ''                not null,
    star_count             int           default 0                 not null,
    item_count             int           default 0                 not null,
    embedding_count        int           default 0                 not null,
    create_time            timestamp     default CURRENT_TIMESTAMP not null,
    update_time            timestamp     default CURRENT_TIMESTAMP not null,
    is_deleted             boolean       default false             not null
);

comment on table adi_knowledge_base is '知识库 | Knowledge Base';
comment on column adi_knowledge_base.title is '知识库名称 | Knowledge Base Title';
comment on column adi_knowledge_base.remark is '知识库描述 | Knowledge Base Description';
comment on column adi_knowledge_base.is_public is '是否公开 | Is Public';
comment on column adi_knowledge_base.is_strict is '是否严格模式,严格模式：严格匹配知识库，知识库中如无搜索结果，直接返回无答案;非严格模式：非严格匹配知识库，知识库中如无搜索结果，将用户提问传给LLM继续请求答案 | Is Strict Mode: Strict mode strictly matches the knowledge base, if there are no search results in the knowledge base, it directly returns no answer; Non-strict mode: Non-strictly matches the knowledge base, if there are no search results in the knowledge base, the question is passed to the LLM for further answers';
comment on column adi_knowledge_base.ingest_max_overlap is '设置文档切块时重叠的最大数量（按token来计），对完整句子切割时才考虑重叠 | Maximum overlap when chunking documents (measured in tokens), only considered when cutting complete sentences';
comment on column adi_knowledge_base.ingest_model_name is '索引(图谱化)文档时使用的LLM,不指定时使用第1个可用的LLM | LLM used when indexing (graphing) documents, if not specified, the first available LLM is used';
comment on column adi_knowledge_base.ingest_model_id is '索引(图谱化)文档时使用的LLM,不指定时使用第1个可用的LLM | LLM ID used when indexing (graphing) documents, if not specified, the first available LLM is used';
comment on column adi_knowledge_base.ingest_token_estimator is '文档切片时需要用到的token数量估计器,默认使用OpenAiTokenizer | Token count estimator, default is OpenAiTokenizer';
comment on column adi_knowledge_base.ingest_embedding_model is '对文档向量化时使用的模型,默认使用all-minilm-l6-v2 | Embedding model for document embedding, default is all-minilm-l6-v2';
comment on column adi_knowledge_base.retrieve_max_results is '设置召回向量最大数量,默认为0,表示由系统根据模型的contentWindow自动调整 | Set the maximum number of recall vectors, default is 0, meaning the system automatically adjusts based on the model''s content window';
comment on column adi_knowledge_base.retrieve_min_score is '设置向量搜索时命中所需的最低分数,为0表示使用默认 | Set the minimum score required for a hit in vector search, 0 means using the default';
comment on column adi_knowledge_base.query_llm_temperature is '用户查询时指定LLM响应时的创造性/随机性 | LLM response creativity/randomness specified during user query';
COMMENT ON COLUMN adi_knowledge_base.query_system_message IS '提供给LLM的系统信息 | System message for LLM';
comment on column adi_knowledge_base.star_count is '点赞数 | Number of Likes';
comment on column adi_knowledge_base.item_count is '知识点数量 | Number of Knowledge Items';
comment on column adi_knowledge_base.embedding_count is '向量数 | Number of Embeddings';
comment on column adi_knowledge_base.owner_id is '所属人id | Owner ID';
comment on column adi_knowledge_base.owner_uuid is '所属人uuid | Owner UUID';
comment on column adi_knowledge_base.owner_name is '所属人名称 | Owner Name';
comment on column adi_knowledge_base.create_time is '创建时间 | Creation Time';
comment on column adi_knowledge_base.update_time is '更新时间 | Update Time';
comment on column adi_knowledge_base.is_deleted is '0：未删除；1：已删除 | Deletion Status, 0: Not Deleted; 1: Deleted';

create trigger trigger_kb_update_time
    before update
    on adi_knowledge_base
    for each row
execute procedure update_modified_column();

create table adi_knowledge_base_item
(
    id                           bigserial primary key,
    uuid                         varchar(32)  default ''                not null,
    kb_id                        bigint       default 0                 not null,
    kb_uuid                      varchar(32)  default ''                not null,
    source_file_id               bigint       default 0                 not null,
    title                        varchar(250) default ''                not null,
    brief                        varchar(250) default ''                not null,
    remark                       text         default ''                not null,
    embedding_status             int          default 1                 not null,
    embedding_status_change_time timestamp    default CURRENT_TIMESTAMP not null,
    graphical_status             int          default 1                 not null,
    graphical_status_change_time timestamp    default CURRENT_TIMESTAMP not null,
    create_time                  timestamp    default CURRENT_TIMESTAMP not null,
    update_time                  timestamp    default CURRENT_TIMESTAMP not null,
    is_deleted                   boolean      default false             not null
);

comment on table adi_knowledge_base_item is '知识库-条目 | Knowledge Base Item';
comment on column adi_knowledge_base_item.kb_id is '所属知识库id | Knowledge Base ID';
comment on column adi_knowledge_base_item.source_file_id is '来源文件id | Source File ID';
comment on column adi_knowledge_base_item.title is '条目标题 | Item Title';
comment on column adi_knowledge_base_item.brief is '条目内容摘要 | Item Brief';
comment on column adi_knowledge_base_item.remark is '条目内容 | Item Content';
comment on column adi_knowledge_base_item.embedding_status is '向量化状态, 1:未向量化,2:正在向量化,3:已向量化,4:失败 | Embedding Status, 1: Not Embedded, 2: Embedding, 3: Embedded, 4: Failed';
comment on column adi_knowledge_base_item.embedding_status_change_time is '向量化状态变更时间 | Embedding Status Change Time';
comment on column adi_knowledge_base_item.graphical_status is '图谱化状态, 1:未图谱化,2:正在图谱化;3:已图谱化,4:失败 | Graphical Status, 1: Not Graphical, 2: Graphing, 3: Graphed, 4: Failed';
comment on column adi_knowledge_base_item.graphical_status_change_time is '图谱化状态变更时间 | Graphical Status Change Time';
comment on column adi_knowledge_base_item.create_time is '创建时间 | Creation Time';
comment on column adi_knowledge_base_item.update_time is '更新时间 | Update Time';
comment on column adi_knowledge_base_item.is_deleted is '0：未删除；1：已删除 | Deletion Status, 0: Not Deleted; 1: Deleted';

create trigger trigger_kb_item_update_time
    before update
    on adi_knowledge_base_item
    for each row
execute procedure update_modified_column();

create table adi_knowledge_base_star
(
    id          bigserial primary key,
    kb_id       bigint      default 0                 not null,
    kb_uuid     varchar(32) default ''                not null,
    user_id     bigint      default '0'               not null,
    user_uuid   varchar(32) default ''                not null,
    create_time timestamp   default CURRENT_TIMESTAMP not null,
    update_time timestamp   default CURRENT_TIMESTAMP not null,
    is_deleted  boolean     default false             not null,
    UNIQUE (kb_id, user_id)
);

comment on table adi_knowledge_base_star is '知识库-点赞记录 | Knowledge Base - Like Records';
comment on column adi_knowledge_base_star.kb_id is '知识库ID | adi_knowledge_base id';
comment on column adi_knowledge_base_star.kb_uuid is '知识库UUID | adi_knowledge_base uuid';
comment on column adi_knowledge_base_star.user_id is '用户ID | adi_user id';
comment on column adi_knowledge_base_star.user_uuid is '用户UUID | adi_user uuid';
comment on column adi_knowledge_base_star.create_time is '创建时间 | Creation Time';
comment on column adi_knowledge_base_star.update_time is '更新时间 | Update Time';
comment on column adi_knowledge_base_star.is_deleted is '是否删除，0: 正常；1: 删除 | Deletion status, 0: Normal; 1: Deleted';

create trigger trigger_kb_star_update_time
    before update
    on adi_knowledge_base_star
    for each row
execute procedure update_modified_column();

create table adi_knowledge_base_qa
(
    id              bigserial primary key,
    uuid            varchar(32)   default ''                not null,
    kb_id           bigint        default 0                 not null,
    kb_uuid         varchar(32)   default ''                not null,
    question        varchar(1000) default ''                not null,
    prompt          text          default ''                not null,
    prompt_tokens   integer       default 0                 not null,
    answer          text          default ''                not null,
    answer_tokens   integer       default 0                 not null,
    source_file_ids varchar(500)  default ''                not null,
    user_id         bigint        default 0                 not null,
    ai_model_id     bigint        default 0                 not null,
    create_time     timestamp     default CURRENT_TIMESTAMP not null,
    update_time     timestamp     default CURRENT_TIMESTAMP not null,
    is_deleted      boolean       default false             not null
);

comment on table adi_knowledge_base_qa is '知识库-提问记录 | Knowledge Base - Question Records';
comment on column adi_knowledge_base_qa.kb_id is '所属知识库id | adi_knowledge_base ID';
comment on column adi_knowledge_base_qa.kb_uuid is '所属知识库uuid | adi_knowledge_base UUID';
comment on column adi_knowledge_base_qa.question is '用户的原始问题 | User''s original question';
comment on column adi_knowledge_base_qa.prompt is '提供给LLM的提示词 | Prompt provided to LLM';
comment on column adi_knowledge_base_qa.prompt_tokens is '提示词消耗的token | Tokens consumed by the prompt';
comment on column adi_knowledge_base_qa.answer is '答案 | Answer';
comment on column adi_knowledge_base_qa.answer_tokens is '答案消耗的token | Tokens consumed by the answer';
comment on column adi_knowledge_base_qa.source_file_ids is '来源文档id,以逗号隔开 | Source file IDs, separated by commas';
comment on column adi_knowledge_base_qa.user_id is '提问用户id | User ID of the questioner';
comment on column adi_knowledge_base_qa.create_time is '创建时间 | Creation Time';
comment on column adi_knowledge_base_qa.update_time is '更新时间 | Update Time';
comment on column adi_knowledge_base_qa.is_deleted is '0：未删除；1：已删除 | Deletion Status, 0: Not Deleted; 1: Deleted';

create trigger trigger_kb_qa_update_time
    before update
    on adi_knowledge_base_qa
    for each row
execute procedure update_modified_column();

create table adi_knowledge_base_qa_ref_embedding
(
    id           bigserial primary key,
    qa_record_id bigint        default 0  not null,
    embedding_id varchar(36)   default '' not null,
    score        numeric(3, 2) default 0  not null,
    user_id      bigint        default 0  not null
);

comment on table adi_knowledge_base_qa_ref_embedding is '知识库-提问记录-向量引用列表 | Knowledge Base - Question Records - Embedding References';
comment on column adi_knowledge_base_qa_ref_embedding.qa_record_id is '提问记录id | adi_knowledge_base_qa ID';
comment on column adi_knowledge_base_qa_ref_embedding.embedding_id is '由消息从向量库中获取到的向量uuid | adi_knowledge_base_embedding UUID';
comment on column adi_knowledge_base_qa_ref_embedding.score is '评分 | Score';
comment on column adi_knowledge_base_qa_ref_embedding.user_id is '所属用户 | User ID';

-- Graph RAG
create table adi_knowledge_base_graph_segment
(
    id           bigserial primary key,
    uuid         varchar(32) default ''                not null,
    kb_uuid      varchar(32) default ''                not null,
    kb_item_uuid varchar(32) default ''                not null,
    remark       text        default ''                not null,
    user_id      bigint      default 0                 not null,
    create_time  timestamp   default CURRENT_TIMESTAMP not null,
    update_time  timestamp   default CURRENT_TIMESTAMP not null,
    is_deleted   boolean     default false             not null
);

comment on table adi_knowledge_base_graph_segment is '知识库-图谱-文本块 | Knowledge Base - Graph Segment';
comment on column adi_knowledge_base_graph_segment.uuid is '唯一标识 | Unique identifier';
comment on column adi_knowledge_base_graph_segment.kb_uuid is '所属知识库uuid |adi_knowledge_base UUID';
comment on column adi_knowledge_base_graph_segment.kb_item_uuid is '所属知识点uuid | adi_knowledge_base_item UUID';
comment on column adi_knowledge_base_graph_segment.remark is '内容 | Content';
comment on column adi_knowledge_base_graph_segment.user_id is '所属用户 | adi_user ID';
comment on column adi_knowledge_base_graph_segment.create_time is '创建时间 | Creation Time';
comment on column adi_knowledge_base_graph_segment.update_time is '更新时间 | Update Time';
comment on column adi_knowledge_base_graph_segment.is_deleted is '是否删除，0：未删除；1：已删除 | Deletion Status, 0: Not Deleted; 1: Deleted';

create trigger trigger_kb_graph_segment_update_time
    before update
    on adi_knowledge_base_graph_segment
    for each row
execute procedure update_modified_column();

create table adi_knowledge_base_qa_ref_graph
(
    id                     bigserial primary key,
    qa_record_id           bigint default 0  not null,
    entities_from_question text   default '' not null, -- 原字段名 graph_from_llm
    graph_from_store       text   default '' not null,
    user_id                bigint default 0  not null
);

comment on table adi_knowledge_base_qa_ref_graph is '知识库-提问记录-图谱引用记录 | Knowledge Base - Question Records - Graph References';
comment on column adi_knowledge_base_qa_ref_graph.qa_record_id is '提问记录id | adi_knowledge_base_qa ID';
comment on column adi_knowledge_base_qa_ref_graph.entities_from_question is '从用户问题中解析出来的实体: vertexName1,vertexName2 | Graph parsed by LLM: vertexName1,vertexName2';
comment on column adi_knowledge_base_qa_ref_graph.graph_from_store is '从图数据库中查找得到的图谱: {vertices:[{id:"111",name:"vertexName1"},{id:"222",name:"vertexName2"}],edges:[{id:"333",name:"edgeName1",start:"111",end:"222"}] | Graph retrieved from graph database: {vertices:[{id:"111",name:"vertexName1"},{id:"222",name:"vertexName2"}],edges:[{id:"333",name:"edgeName1",start:"111",end:"222"}]';
comment on column adi_knowledge_base_qa_ref_graph.user_id is '所属用户 | adi_user ID';

-- ============================================================
-- Workflow: workflow definitions, components, runtime
-- ============================================================
create table adi_workflow_component
(
    id            bigserial primary key,
    uuid          varchar(32)  default ''                not null,
    name          varchar(32)  default ''                not null,
    title         varchar(100) default ''                not null,
    remark        text         default ''                not null,
    display_order int          default 0                 not null,
    is_enable     boolean      default false             not null,
    create_time   timestamp    default CURRENT_TIMESTAMP not null,
    update_time   timestamp    default CURRENT_TIMESTAMP not null,
    is_deleted    boolean      default false             not null
);
create trigger trigger_workflow_component
    before update
    on adi_workflow_component
    for each row
execute procedure update_modified_column();

-- 工作流定义（用户定义的工作流）| Workflow Definition (User-defined Workflow)
create table adi_workflow
(
    id          bigserial primary key,
    uuid        varchar(32)  default ''                not null,
    title       varchar(100) default ''                not null,
    remark      text         default ''                not null,
    user_id     bigint       default 0                 not null,
    is_public   boolean      default false             not null,
    is_enable   boolean      default true              not null,
    create_time timestamp    default CURRENT_TIMESTAMP not null,
    update_time timestamp    default CURRENT_TIMESTAMP not null,
    is_deleted  boolean      default false             not null
);
-- definition
-- template
comment on table adi_workflow is '工作流定义（用户定义的工作流）| Workflow Definition (User-defined Workflow)';
create trigger trigger_workflow
    before update
    on adi_workflow
    for each row
execute procedure update_modified_column();

create table adi_workflow_node
(
    id                    bigserial primary key,
    uuid                  varchar(32)      default ''                not null,
    workflow_id           bigint           default 0                 not null,
    workflow_component_id bigint           default 0                 not null,
    user_id               bigint           default 0                 not null,
    title                 varchar(100)     default ''                not null,
    remark                varchar(500)     default ''                not null,
    input_config          jsonb            default '{}'              not null,
    node_config           jsonb            default '{}'              not null,
    position_x            double precision default 0                 not null,
    position_y            double precision default 0                 not null,
    create_time           timestamp        default CURRENT_TIMESTAMP not null,
    update_time           timestamp        default CURRENT_TIMESTAMP not null,
    is_deleted            boolean          default false             not null
);
comment on table adi_workflow_node is '工作流定义的节点 | Node of Workflow Definition';
comment on column adi_workflow_node.input_config is '{"params":[{"name":"user_define_param01","type":"string"}]}';
comment on column adi_workflow_node.node_config is '{"params":[{"prompt":"Summarize the following content:{user_define_param01}"}]}';
create trigger trigger_workflow_node
    before update
    on adi_workflow_node
    for each row
execute procedure update_modified_column();

create table adi_workflow_edge
(
    id               bigserial primary key,
    uuid             varchar(32) default ''                not null,
    workflow_id      bigint      default 0                 not null,
    source_node_uuid varchar(32) default ''                not null,
    source_handle    varchar(32) default ''                not null,
    target_node_uuid varchar(32) default ''                not null,
    create_time      timestamp   default CURRENT_TIMESTAMP not null,
    update_time      timestamp   default CURRENT_TIMESTAMP not null,
    is_deleted       boolean     default false             not null
);
create trigger trigger_workflow_edge
    before update
    on adi_workflow_edge
    for each row
execute procedure update_modified_column();

-- 工作流实例（运行时）| Workflow Runtime
create table adi_workflow_runtime
(
    id            bigserial primary key,
    uuid          varchar(32)  default ''                not null,
    user_id       bigint       default 0                 not null,
    workflow_id   bigint       default 0                 not null,
    input         jsonb        default '{}'              not null,
    output        jsonb        default '{}'              not null,
    status        smallint     default 1                 not null,
    status_remark varchar(250) default ''                not null,
    create_time   timestamp    default CURRENT_TIMESTAMP not null,
    update_time   timestamp    default CURRENT_TIMESTAMP not null,
    is_deleted    boolean      default false             not null
);
COMMENT ON COLUMN adi_workflow_runtime.input IS '{"userInput01":"text01","userInput02":true,"userInput03":10,"userInput04":["selectedA","selectedB"],"userInput05":["https://a.com/a.xlxs","https://a.com/b.png"]}';
COMMENT ON COLUMN adi_workflow_runtime.status IS '执行状态，1：就绪，2：执行中，3：成功，4：失败 | Execution status, 1: Ready, 2: In progress, 3: Success, 4: Failed';
COMMENT ON COLUMN adi_workflow_runtime.status_remark IS '状态备注 | Status remark';
create trigger trigger_workflow_runtime
    before update
    on adi_workflow_runtime
    for each row
execute procedure update_modified_column();

-- 工作流实例（运行时）- 节点| Workflow Runtime - node
create table adi_workflow_runtime_node
(
    id                  bigserial primary key,
    uuid                varchar(32)  default ''                not null,
    user_id             bigint       default 0                 not null,
    workflow_runtime_id bigint       default 0                 not null,
    node_id             bigint       default 0                 not null,
    input               jsonb        default '{}'              not null,
    output              jsonb        default '{}'              not null,
    status              smallint     default 1                 not null,
    status_remark       varchar(250) default ''                not null,
    create_time         timestamp    default CURRENT_TIMESTAMP not null,
    update_time         timestamp    default CURRENT_TIMESTAMP not null,
    is_deleted          boolean      default false             not null
);
COMMENT ON COLUMN adi_workflow_runtime_node.status IS '执行状态，1：进行中，2：失败，3：成功 | Execution status, 1: In progress, 2: Failed, 3: Success';
COMMENT ON COLUMN adi_workflow_runtime_node.status_remark IS '状态备注 | Status remark';
create trigger trigger_workflow_runtime_node
    before update
    on adi_workflow_runtime_node
    for each row
execute procedure update_modified_column();

-- ============================================================
-- MCP: model context protocol services
-- ============================================================

create table adi_mcp
(
    id                           bigserial primary key,
    uuid                         varchar(32)   default ''                not null,
    title                        varchar(100)  default ''                not null,
    transport_type               varchar(25)   default ''                not null,
    sse_url                      varchar(250)  default ''                not null,
    sse_timeout                  int           default 0                 not null,
    stdio_command                varchar(200)  default ''                not null,
    stdio_arg                    varchar(1024) default ''                not null,
    preset_params                jsonb         default '[]'              not null,
    customized_param_definitions jsonb         default '[]'              not null,
    install_type                 varchar(25)   default ''                not null,
    website                      varchar(250)  default ''                not null,
    remark                       text          default ''                not null,
    is_enable                    boolean       default false             not null,
    create_time                  timestamp     default CURRENT_TIMESTAMP not null,
    update_time                  timestamp     default CURRENT_TIMESTAMP not null,
    is_deleted                   boolean       default false             not null
);
COMMENT ON TABLE adi_mcp is 'MCP server模板定义 | MCP server template';
COMMENT ON COLUMN adi_mcp.transport_type IS '传输类型：1:sse、2:stdio | Transport type: 1:sse、2:stdio';
COMMENT ON COLUMN adi_mcp.preset_params IS '由系统管理员预设的参数,如:[{"name":"BAIDU_MAP_API_KEY","title":"百度地图服务","value":"111111","require_encrypt":true,"encrypted":true}] | Parameters preset by the system administrator, e.g., [{"name":"BAIDU_MAP_API_KEY","title":"Baidu Map Service","value":"111111","require_encrypt":true,"encrypted":true}]';
COMMENT ON COLUMN adi_mcp.customized_param_definitions IS '待用户设置的参数定义,用户设置后与preset_params合并做为mcp的启动参数,格式:[{"name":"GITHUB_PERSONAL_ACCESS_TOKEN","title":"github access token","require_encrypt":true}] | Parameters to be set by the user, after user settings, merged with preset_params as MCP startup parameters, format: [{"name":"GITHUB_PERSONAL_ACCESS_TOKEN","title":"github access token","require_encrypt":true}]';
COMMENT ON COLUMN adi_mcp.install_type IS 'mcp server的安装方式, 1:docker、2:local、3:remote、4:wasm | Installation type of mcp server: 1:docker, 2:local, 3:remote, 4:wasm';
COMMENT ON COLUMN adi_mcp.remark IS '描述，支持markdown格式 | Supports markdown format';
COMMENT ON COLUMN adi_mcp.website IS '官网地址 | Official website';

create trigger trigger_mcp
    before update
    on adi_mcp
    for each row
execute procedure update_modified_column();

create table adi_user_mcp
(
    id                    bigserial primary key,
    uuid                  varchar(32) default ''                not null,
    user_id               bigint      default 0                 not null,
    mcp_id                bigint      default 0                 not null,
    mcp_customized_params jsonb       default '[]'              not null,
    is_enable             boolean     default false             not null,
    create_time           timestamp   default CURRENT_TIMESTAMP not null,
    update_time           timestamp   default CURRENT_TIMESTAMP not null,
    is_deleted            boolean     default false             not null
);
comment on table adi_user_mcp is '用户启用的mcp server及配置 | User-enabled MCP server and configuration';
COMMENT ON COLUMN adi_user_mcp.mcp_customized_params IS '用户设置的mcp参数(个性化配置)，对应了adi_mcp.customized_param_definitions中定义的参数，格式：[{"name","BAIDU_MAP_API_KEY","value":"111111",encrypted:true}] | User-defined MCP variables, corresponding to the variables defined in adi_mcp.customized_param_definitions, format: [{"name","BAIDU_MAP_API_KEY","value":"111111",encrypted:true}]';

create trigger trigger_user_mcp
    before update
    on adi_user_mcp
    for each row
execute procedure update_modified_column();

