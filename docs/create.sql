-- 安装pgvector扩展（https://github.com/pgvector/pgvector）
-- 安装Apache AGE扩展（https://github.com/apache/age）
-- CREATE EXTENSION IF NOT EXISTS vector;
-- CREATE EXTENSION IF NOT EXISTS age;

SET client_encoding = 'UTF8';
CREATE SCHEMA public;

-- update_time trigger
CREATE OR REPLACE FUNCTION update_modified_column()
    RETURNS TRIGGER AS
$$
BEGIN
    NEW.update_time = CURRENT_TIMESTAMP;
    RETURN NEW;
END;
$$ language 'plpgsql';

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

-- 旧版本（3.15.0及以下）中本表 adi_model_platform 的数据位于 adi_sys_config 中
-- 需要手动将 adi_sys_config 中对应的模型平台配置项移动到 adi_model_platform 表中
-- 需要迁移的配置为：deepseek_setting、openai_setting、dashscope_setting、qianfan_setting、ollama_setting、siliconflow_setting
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
COMMENT ON COLUMN adi_model_platform.name IS '模型平台名称，如openai, dashscope, qianfan, ollama | Model provider name, e.g., openai, dashscope, qianfan, ollama';
COMMENT ON COLUMN adi_model_platform.title IS '模型平台标题，可读性更高的名称，如: OpenAI，DeepSeek深度求索 | Model provider title, a more readable name, e.g., OpenAI, DeepSeek';
COMMENT ON COLUMN adi_model_platform.base_url IS '模型平台的API请求地址 | API base URL of the model provider';
COMMENT ON COLUMN adi_model_platform.api_key IS '模型平台的API Key | API Key of the model provider';
COMMENT ON COLUMN adi_model_platform.secret_key IS '模型平台的Secret Key，目前只有百度的千帆在用，其他供应商的key直接放到 api_key 即可 | Secret Key of the model provider';
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
COMMENT ON COLUMN adi_ai_model.platform IS '平台，对应了 adi_model_platform.name | Model platform (as model provider): openai, dashscope, qianfan, ollama';
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

-- ai search
create table adi_ai_search_record
(
    id                     bigserial primary key,
    uuid                   varchar(32)   default ''                not null,
    question               varchar(1000) default ''                not null,
    search_engine_response jsonb                                   not null,
    prompt                 text          default ''                not null,
    prompt_tokens          integer       default 0                 not null,
    answer                 text          default ''                not null,
    answer_tokens          integer       default 0                 not null,
    user_id                bigint        default 0                 not null,
    user_uuid              varchar(32)   default ''                not null,
    ai_model_id            bigint        default 0                 not null,
    create_time            timestamp     default CURRENT_TIMESTAMP not null,
    update_time            timestamp     default CURRENT_TIMESTAMP not null,
    is_deleted             boolean       default false             not null
);
comment on table adi_ai_search_record is '搜索记录 | Search record';
comment on column adi_ai_search_record.question is '用户的原始问题 | User original question';
comment on column adi_ai_search_record.search_engine_response is '搜索引擎的响应内容 | Search engine''s response content';
comment on column adi_ai_search_record.prompt is 'LLM的提示词 | Prompt of LLM';
comment on column adi_ai_search_record.prompt_tokens is '提示词消耗的token数量 | Tokens consumed by the prompt';
comment on column adi_ai_search_record.answer is 'LLM的响应 | LLM response';
comment on column adi_ai_search_record.answer_tokens is 'LLM响应消耗的token数量 | Tokens consumed by the LLM response';
comment on column adi_ai_search_record.user_id is '用户ID | adi_user ID';
comment on column adi_ai_search_record.ai_model_id is 'AI模型ID | adi_ai_model ID';
comment on column adi_ai_search_record.create_time is '创建时间 | Creation time';
comment on column adi_ai_search_record.update_time is '更新时间 | Update time';
comment on column adi_ai_search_record.is_deleted is '是否删除，0: 正常；1: 删除 | Deletion status, 0: Normal; 1: Deleted';

create trigger trigger_ai_search_record
    before update
    on adi_ai_search_record
    for each row
execute procedure update_modified_column();

-- workflow
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

-- 初始化数据 --

-- 管理员账号：catkeeper@aideepin.com  密码：123456
INSERT INTO adi_user (name, password, uuid, email, user_status, is_admin)
VALUES ('catkeeper', '$2a$10$z44gncmQk6xCBCeDx55gMe1Zc8uYtOKcoT4/HE2F92VcF7wP2iquG',
        replace(gen_random_uuid()::text, '-', ''), 'catkeeper@aideepin.com', 2, true);

-- 配置信息
-- https://api-docs.deepseek.com/zh-cn/
INSERT INTO adi_sys_config (name, value)
VALUES ('google_setting',
        '{"url":"https://www.googleapis.com/customsearch/v1","key":"","cx":""}');
INSERT INTO adi_sys_config (name, value)
VALUES ('request_text_rate_limit', '{"times":24,"minutes":3}');
INSERT INTO adi_sys_config (name, value)
VALUES ('request_image_rate_limit', '{"times":6,"minutes":3}');
INSERT INTO adi_sys_config (name, value)
VALUES ('conversation_max_num', '50');
INSERT INTO adi_sys_config (name, value)
VALUES ('quota_by_token_daily', '10000');
INSERT INTO adi_sys_config (name, value)
VALUES ('quota_by_token_monthly', '200000');
INSERT INTO adi_sys_config (name, value)
VALUES ('quota_by_request_daily', '150');
INSERT INTO adi_sys_config (name, value)
VALUES ('quota_by_request_monthly', '3000');
INSERT INTO adi_sys_config (name, value)
VALUES ('quota_by_image_daily', '30');
INSERT INTO adi_sys_config (name, value)
VALUES ('quota_by_image_monthly', '300');
INSERT INTO adi_sys_config (name, value)
VALUES ('quota_by_qa_ask_daily', '50');
INSERT INTO adi_sys_config (name, value)
VALUES ('quota_by_qa_item_monthly', '100');

-- 文件存储位置，默认存储到本地
-- store_location: 1表示存储到本地,2表示存储到阿里云oss
INSERT INTO adi_sys_config (name, value)
VALUES ('storage_location', '1');
-- endpoint: 如：oss-cn-hangzhou.aliyuncs.com
INSERT INTO adi_sys_config (name, value)
VALUES ('storage_location_ali_oss', '{"access_key_id":"","access_key_secret":"","endpoint":"","bucket_name":""}');

-- ASR设置
-- 音频文件最大10MB，最大录音时长60秒（TODO）
INSERT INTO adi_sys_config (name, value)
VALUES ('asr_setting',
        '{"model_name":"FunAudioLLM/SenseVoiceSmall","platform":"siliconflow","max_record_duration":60,"max_file_size":10485760}');

-- TTS设置
-- synthesizer_side指定TTS的合成器类型，如: client,server
-- synthesizer_side如果设置为client，表示使用客户端（如浏览器）的tts功能，忽略model_name, platform等参数，免费使用
-- synthesizer_side如果设置为server，表示使用服务端进行语音合成，实际上就是使用各大平台的大语言模型如cosyvoice-v2,FunAudioLLM/CosyVoice2-0.5B等进行合成，付费使用
-- 注意：大语言模型的TTS接口收费不便宜，请提前做好规划
INSERT INTO adi_sys_config (name, value)
VALUES ('tts_setting', '{"synthesizer_side":"client","model_name":"","platform":""}');
-- INSERT INTO adi_sys_config (name, value)
-- VALUES ('tts_setting', '{"synthesizer":"server","model_name":"cosyvoice-v2","platform":"dashscope"}');

-- 模型平台
insert into adi_model_platform (name, title, base_url)
values ('openai', 'OpenAi', 'https://api.openai.com/v1');
insert into adi_model_platform (name, title, base_url)
values ('deepseek', 'DeepSeek深度求索', 'https://api.deepseek.com/v1');
insert into adi_model_platform (name, title, base_url)
values ('dashscope', 'DashScope', 'https://dashscope.aliyuncs.com/api/v1');
insert into adi_model_platform (name, title, base_url)
values ('siliconflow', '硅基流动', 'https://api.siliconflow.cn/v1');
insert into adi_model_platform (name, title, base_url)
values ('ollama', 'ollama', 'http://localhost:11434');
insert into adi_model_platform (name, title, base_url)
values ('qianfan', '千帆', '');

-- 硅基流动的文本模型的api兼容 openai api，本行数据用来测试动态创建的模型平台及模型是否正常使用了 OpenAiCompatibleLLMService 进行请求
insert into adi_model_platform (name, title, base_url, is_openai_api_compatible)
values ('openai-compatible-platform-test', '兼容openai的平台', 'https://api.siliconflow.cn/v1', true);

-- 大语言模型
-- https://api-docs.deepseek.com/zh-cn/quick_start/pricing
INSERT INTO adi_ai_model (name, title, type, platform, context_window, max_input_tokens, max_output_tokens,
                          response_format_types, is_enable)
VALUES ('deepseek-chat', 'DeepSeek-V3', 'text', 'deepseek', 131072, 126976, 8192, 'text,json_object', false);
INSERT INTO adi_ai_model (name, title, type, platform, context_window, max_input_tokens, max_output_tokens,
                          response_format_types, is_reasoner,
                          is_thinking_closable, is_enable)
VALUES ('deepseek-reasoner', 'DeepSeek-R1', 'text', 'deepseek', 131072, 65536, 65536, 'text,json_object', true, false,
        false);

-- https://platform.openai.com/docs/models/gpt-5-mini
INSERT INTO adi_ai_model (name, title, type, platform, context_window, max_input_tokens, max_output_tokens,
                          response_format_types, remark,
                          is_enable)
VALUES ('gpt-5-mini', 'gpt-5-mini', 'text', 'openai', 400000, 272000, 128000, 'text,json_object',
        'GPT-5 mini is a faster, more cost-efficient version of GPT-5. It''s great for well-defined tasks and precise prompts.',
        false);

-- use gpt-5-mini in place of GPT-3.5 Turbo
-- https://platform.openai.com/docs/models/gpt-3-5-turbo
-- INSERT INTO adi_ai_model (name, title, type, platform, context_window, max_input_tokens, max_output_tokens, is_enable)
-- VALUES ('gpt-3.5-turbo', 'gpt3.5', 'text', 'openai', 16385, 12385, 4096, false);

INSERT INTO adi_ai_model (name, title, type, platform, is_enable)
VALUES ('dall-e-2', 'DALL-E-2', 'image', 'openai', false);
INSERT INTO adi_ai_model (name, title, type, platform, is_enable)
VALUES ('dall-e-3', 'DALL-E-3', 'image', 'openai', false);
INSERT INTO adi_ai_model (name, title, type, platform, max_input_tokens, properties, is_enable)
VALUES ('text-embedding-3-small', 'openai-embedding-small', 'embedding', 'openai', 8191, '{
  "dimension": 1536
}', false);
INSERT INTO adi_ai_model (name, title, type, platform, max_input_tokens, properties, is_enable)
VALUES ('text-embedding-3-large', 'openai-embedding-large', 'embedding', 'openai', 8191, '{
  "dimension": 3072
}', false);
-- https://help.aliyun.com/zh/dashscope/developer-reference/model-introduction?spm=a2c4g.11186623.0.i39
INSERT INTO adi_ai_model (name, title, type, platform, context_window, max_input_tokens, max_output_tokens,
                          response_format_types, is_support_web_search, is_reasoner,
                          is_thinking_closable, is_enable)
VALUES ('qwen-turbo', '通义千问turbo', 'text', 'dashscope', 131072, 98304, 16384, 'text,json_object', true, true, true,
        false);
-- 图片识别
INSERT INTO adi_ai_model (name, title, type, platform, context_window, max_input_tokens, max_output_tokens, input_types,
                          is_enable)
VALUES ('qwen2-vl-7b-instruct', '通义千问-识图', 'vision', 'dashscope', 32768, 16384, 16384, 'text,image', false);
-- https://help.aliyun.com/zh/model-studio/developer-reference/text-to-image-v2-api-reference?spm=a2c4g.11186623.0.i2
-- 通义万相-文生图（wanx2.1-t2i-plus、wanx2.1-t2i-turbo）
INSERT INTO adi_ai_model (name, title, type, platform, is_enable)
VALUES ('wanx2.1-t2i-turbo', '通义万相-文生图', 'image', 'dashscope', false);
-- 通义万相-切换背景
INSERT INTO adi_ai_model (name, title, type, platform, input_types, is_enable)
VALUES ('wanx-background-generation-v2', '通义万相-背景生成', 'image', 'dashscope', 'text,image', false);
-- 通义千问-向量（可选模型名：text-embedding-v1,text-embedding-v2,text-embedding-v3）
INSERT INTO adi_ai_model (name, title, type, platform, max_input_tokens, properties, is_enable)
VALUES ('text-embedding-v1', '通义千问-embedding-v1', 'embedding', 'dashscope', 2048, '{
  "dimension": 1536
}', false);
INSERT INTO adi_ai_model (name, title, type, platform, max_input_tokens, properties, is_enable)
VALUES ('text-embedding-v3', '通义千问-embedding-v3', 'embedding', 'dashscope', 8192, '{
  "dimension": 1024
}', false);
-- 语音识别
-- paraformer-v2 只支持公网可访问的音频文件，如需要传输本地音频文件，请激活使用下面硅基流动的SenseVoiceSmall
INSERT INTO adi_ai_model (name, title, type, platform, input_types, is_enable)
VALUES ('paraformer-v2', '通义-语音识别ASR', 'asr', 'dashscope', 'audio', false);
-- 语音合成
INSERT INTO adi_ai_model (name, title, type, platform, input_types, properties, is_enable)
VALUES ('cosyvoice-v2', '通义-语音合成TTS', 'tts', 'dashscope', 'text', '{
  "voices": [
    {
      "name": "龙应催",
      "remark": "严肃催收男",
      "param_name": "longyingcui",
      "lang": "中、英"
    },
    {
      "name": "龙应答",
      "remark": "开朗高音女",
      "param_name": "longyingda",
      "lang": "中、英"
    },
    {
      "name": "龙应静",
      "remark": "低调冷静女",
      "param_name": "longyingjing",
      "lang": "中、英"
    },
    {
      "name": "龙应严",
      "remark": "义正严辞女",
      "param_name": "longyingyan",
      "lang": "中、英"
    },
    {
      "name": "龙应甜",
      "remark": "温柔甜美女",
      "param_name": "longyingtian",
      "lang": "中、英"
    },
    {
      "name": "龙应冰",
      "remark": "尖锐强势女",
      "param_name": "longyingbing",
      "lang": "中、英"
    },
    {
      "name": "龙应桃",
      "remark": "温柔淡定女",
      "param_name": "longyingtao",
      "lang": "中、英"
    },
    {
      "name": "龙应聆",
      "remark": "温和共情女",
      "param_name": "longyingling",
      "lang": "中、英"
    },
    {
      "name": "YUMI",
      "remark": "正经青年女",
      "param_name": "longyumi_v2",
      "lang": "中、英"
    },
    {
      "name": "龙小淳",
      "remark": "知性积极女",
      "param_name": "longxiaochun_v2",
      "lang": "中、英"
    },
    {
      "name": "龙小夏",
      "remark": "沉稳权威女",
      "param_name": "longxiaoxia_v2",
      "lang": "中、英"
    },
    {
      "name": "龙安燃",
      "remark": "活泼质感女",
      "param_name": "longanran",
      "lang": "中、英"
    },
    {
      "name": "龙安宣",
      "remark": "经典直播女",
      "param_name": "longanxuan",
      "lang": "中、英"
    },
    {
      "name": "龙三叔",
      "remark": "沉稳质感男",
      "param_name": "longsanshu",
      "lang": "中、英"
    },
    {
      "name": "龙修",
      "remark": "博才说书男",
      "param_name": "longxiu_v2",
      "lang": "中、英"
    },
    {
      "name": "龙妙",
      "remark": "抑扬顿挫女",
      "param_name": "longmiao_v2",
      "lang": "中、英"
    },
    {
      "name": "龙悦",
      "remark": "温暖磁性女",
      "param_name": "longyue_v2",
      "lang": "中、英"
    },
    {
      "name": "龙楠",
      "remark": "睿智青年男",
      "param_name": "longnan_v2",
      "lang": "中、英"
    },
    {
      "name": "龙媛",
      "remark": "温暖治愈女",
      "param_name": "longyuan_v2",
      "lang": "中、英"
    },
    {
      "name": "龙安柔",
      "remark": "温柔闺蜜女",
      "param_name": "longanrou",
      "lang": "中、英"
    },
    {
      "name": "龙嫱",
      "remark": "浪漫风情女",
      "param_name": "longqiang_v2",
      "lang": "中、英"
    },
    {
      "name": "龙寒",
      "remark": "温暖痴情男",
      "param_name": "longhan_v2",
      "lang": "中、英"
    },
    {
      "name": "龙星",
      "remark": "温婉邻家女",
      "param_name": "longxing_v2",
      "lang": "中、英"
    },
    {
      "name": "龙华",
      "remark": "元气甜美女",
      "param_name": "longhua_v2",
      "lang": "中、英"
    },
    {
      "name": "龙婉",
      "remark": "积极知性女",
      "param_name": "longwan_v2",
      "lang": "中、英"
    },
    {
      "name": "龙橙",
      "remark": "智慧青年男",
      "param_name": "longcheng_v2",
      "lang": "中、英"
    },
    {
      "name": "龙菲菲",
      "remark": "甜美娇气女",
      "param_name": "longfeifei_v2",
      "lang": "中、英"
    },
    {
      "name": "龙小诚",
      "remark": "磁性低音男",
      "param_name": "longxiaocheng_v2",
      "lang": "中、英"
    },
    {
      "name": "龙哲",
      "remark": "呆板大暖男",
      "param_name": "longzhe_v2",
      "lang": "中、英"
    },
    {
      "name": "龙颜",
      "remark": "温暖春风女",
      "param_name": "longyan_v2",
      "lang": "中、英"
    },
    {
      "name": "龙天",
      "remark": "磁性理智男",
      "param_name": "longtian_v2",
      "lang": "中、英"
    },
    {
      "name": "龙泽",
      "remark": "温暖元气男",
      "param_name": "longze_v2",
      "lang": "中、英"
    },
    {
      "name": "龙邵",
      "remark": "积极向上男",
      "param_name": "longshao_v2",
      "lang": "中、英"
    },
    {
      "name": "龙浩",
      "remark": "多情忧郁男",
      "param_name": "longhao_v2",
      "lang": "中、英"
    },
    {
      "name": "龙深",
      "remark": "实力歌手男",
      "param_name": "kabuleshen_v2",
      "lang": "中、英"
    },
    {
      "name": "龙杰力豆",
      "remark": "阳光顽皮男",
      "param_name": "longjielidou_v2",
      "lang": "中、英"
    },
    {
      "name": "龙铃",
      "remark": "稚气呆板女",
      "param_name": "longling_v2",
      "lang": "中、英"
    },
    {
      "name": "龙可",
      "remark": "懵懂乖乖女",
      "param_name": "longke_v2",
      "lang": "中、英"
    },
    {
      "name": "龙仙",
      "remark": "豪放可爱女",
      "param_name": "longxian_v2",
      "lang": "中、英"
    },
    {
      "name": "龙老铁",
      "remark": "东北直率男",
      "param_name": "longlaotie_v2",
      "lang": "中（东北）、英"
    },
    {
      "name": "龙嘉怡",
      "remark": "知性粤语女",
      "param_name": "longjiayi_v2",
      "lang": "中（粤语）、英"
    },
    {
      "name": "龙桃",
      "remark": "积极粤语女",
      "param_name": "longtao_v2",
      "lang": "中（粤语）、英"
    },
    {
      "name": "龙飞",
      "remark": "热血磁性男",
      "param_name": "longfei_v2",
      "lang": "中、英"
    },
    {
      "name": "李白",
      "remark": "古代诗仙男",
      "param_name": "libai_v2",
      "lang": "中、英"
    },
    {
      "name": "龙津",
      "remark": "优雅温润男",
      "param_name": "longjin_v2",
      "lang": "中、英"
    },
    {
      "name": "龙书",
      "remark": "沉稳青年男",
      "param_name": "longshu_v2",
      "lang": "中、英"
    },
    {
      "name": "Bella2.0",
      "remark": "精准干练女",
      "param_name": "loongbella_v2",
      "lang": "中、英"
    },
    {
      "name": "龙硕",
      "remark": "博才干练男",
      "param_name": "longshuo_v2",
      "lang": "中、英"
    },
    {
      "name": "龙小白",
      "remark": "沉稳播报女",
      "param_name": "longxiaobai_v2",
      "lang": "中、英"
    },
    {
      "name": "龙婧",
      "remark": "典型播音女",
      "param_name": "longjing_v2",
      "lang": "中、英"
    },
    {
      "name": "loongstella",
      "remark": "飒爽利落女",
      "param_name": "loongstella_v2",
      "lang": "中、英"
    },
    {
      "name": "loongeva",
      "remark": "知性英文女",
      "param_name": "loongeva_v2",
      "lang": "英式英文"
    },
    {
      "name": "loongbrian",
      "remark": "沉稳英文男",
      "param_name": "loongbrian_v2",
      "lang": "英式英文"
    },
    {
      "name": "loongluna",
      "remark": "英式英文女",
      "param_name": "loongluna_v2",
      "lang": "英式英文"
    },
    {
      "name": "loongluca",
      "remark": "英式英文男",
      "param_name": "loongluca_v2",
      "lang": "英式英文"
    },
    {
      "name": "loongemily",
      "remark": "英式英文女",
      "param_name": "loongemily_v2",
      "lang": "英式英文"
    },
    {
      "name": "loongeric",
      "remark": "英式英文男",
      "param_name": "loongeric_v2",
      "lang": "英式英文"
    },
    {
      "name": "loongabby",
      "remark": "美式英文女",
      "param_name": "loongabby_v2",
      "lang": "美式英文"
    },
    {
      "name": "loongannie",
      "remark": "美式英文女",
      "param_name": "loongannie_v2",
      "lang": "美式英文"
    },
    {
      "name": "loongandy",
      "remark": "美式英文男",
      "param_name": "loongandy_v2",
      "lang": "美式英文"
    },
    {
      "name": "loongava",
      "remark": "美式英文女",
      "param_name": "loongava_v2",
      "lang": "美式英文"
    },
    {
      "name": "loongbeth",
      "remark": "美式英文女",
      "param_name": "loongbeth_v2",
      "lang": "美式英文"
    },
    {
      "name": "loongbetty",
      "remark": "美式英文女",
      "param_name": "loongbetty_v2",
      "lang": "美式英文"
    },
    {
      "name": "loongcindy",
      "remark": "美式英文女",
      "param_name": "loongcindy_v2",
      "lang": "美式英文"
    },
    {
      "name": "loongcally",
      "remark": "美式英文女",
      "param_name": "loongcally_v2",
      "lang": "美式英文"
    },
    {
      "name": "loongdavid",
      "remark": "美式英文男",
      "param_name": "loongdavid_v2",
      "lang": "美式英文"
    },
    {
      "name": "loongdonna",
      "remark": "美式英文女",
      "param_name": "loongdonna_v2",
      "lang": "美式英文"
    },
    {
      "name": "loongkyong",
      "remark": "韩语女",
      "param_name": "loongkyong_v2",
      "lang": "韩语"
    },
    {
      "name": "loongtomoka",
      "remark": "日语女",
      "param_name": "loongtomoka_v2",
      "lang": "日语"
    },
    {
      "name": "loongtomoya",
      "remark": "日语男",
      "param_name": "loongtomoya_v2",
      "lang": "日语"
    }
  ]
}', false);
-- https://console.bce.baidu.com/qianfan/modelcenter/model/buildIn/detail/am-bg7n2rn2gsbb
INSERT INTO adi_ai_model (name, title, type, platform, context_window, max_input_tokens, max_output_tokens, is_free,
                          is_enable,
                          setting)
VALUES ('ERNIE-Speed-128K', 'ernie_speed', 'text', 'qianfan', 131072, 126976, 4096, true, false,
        '{"endpoint":"ernie-speed-128k"}');
INSERT INTO adi_ai_model (name, title, type, platform, is_enable)
VALUES ('tinydolphin', 'ollama-tinydolphin', 'text', 'ollama', false);
INSERT INTO adi_ai_model (name, title, type, platform, response_format_types, remark, is_free, is_enable)
VALUES ('THUDM/GLM-Z1-9B-0414', '硅基流动-GLM-Z1-9B', 'text', 'siliconflow', 'text,json_object',
        'GLM-Z1-9B-0414 是 GLM 系列的小型模型，仅有 90 亿参数，但保持了开源传统的同时展现出惊人的能力。', true, false);
INSERT INTO adi_ai_model (name, title, type, platform, input_types, remark, is_free, is_enable)
VALUES ('THUDM/GLM-4.1V-9B-Thinking', '硅基流动-识图', 'vision', 'siliconflow', 'text,image',
        'GLM-4.1V-9B-Thinking 是由智谱 AI 和清华大学 KEG 实验室联合发布的一款开源视觉语言模型（VLM），专为处理复杂的多模态认知任务而设计。该模型基于 GLM-4-9B-0414 基础模型，通过引入“思维链”（Chain-of-Thought）推理机制和采用强化学习策略，显著提升了其跨模态的推理能力和稳定性。作为一个 9B 参数规模的轻量级模型，它在部署效率和性能之间取得了平衡，在 28 项权威评测基准中，有 18 项的表现持平甚至超越了 72B 参数规模的 Qwen-2.5-VL-72B。该模型不仅在图文理解、数学科学推理、视频理解等任务上表现卓越，还支持高达 4K 分辨率的图像和任意宽高比输入',
        true, false);

INSERT INTO adi_ai_model (name, title, type, platform, properties, remark, is_free, is_enable)
VALUES ('Kwai-Kolors/Kolors', '硅基流动-文生图', 'image', 'siliconflow', '{
  "image_sizes": [
    "1024x1024",
    "960x1280",
    "768x1024",
    "720x1440",
    "720x1280"
  ]
}',
        'Kolors 是由快手 Kolors 团队开发的基于潜在扩散的大规模文本到图像生成模型。该模型通过数十亿文本-图像对的训练，在视觉质量、复杂语义准确性以及中英文字符渲染方面展现出显著优势。它不仅支持中英文输入，在理解和生成中文特定内容方面也表现出色。',
        true, false);

-- Qwen/Qwen-Image 为收费模型，详细评估后再判断是否要启用
-- INSERT INTO adi_ai_model (name, title, type, platform, properties, remark, is_free, is_enable)
-- VALUES ('Qwen/Qwen-Image', '硅基流动-文生图（qwen）', 'image', 'siliconflow', '{
--   "image_size": [
--     "1328x1328",
--     "1664x928",
--     "928x1664",
--     "1472x1140",
--     "1140x1472",
--     "1584x1056",
--     "1056x1584"
--   ]
-- }',
--         'Qwen-Image 是由阿里巴巴通义千问团队发布的图像生成基础模型，拥有 200 亿参数。该模型在复杂的文本渲染和精确的图像编辑方面取得了显著进展，尤其擅长生成包含高保真度中英文文字的图像。', false, false);

-- test data
INSERT INTO adi_ai_model (name, title, type, platform, response_format_types, is_enable)
VALUES ('THUDM/GLM-Z1-9B-0414', 'openai-compatible-model-test', 'text', 'openai-compatible-platform-test',
        'text,json_object', false);
-- test data end


-- 语音识别
INSERT INTO adi_ai_model (name, title, type, platform, input_types, response_format_types, is_free, is_enable)
VALUES ('FunAudioLLM/SenseVoiceSmall', '硅基流动-语音识别', 'asr', 'siliconflow', 'audio', 'text,json_object', true,
        true);
-- 语音合成
INSERT INTO adi_ai_model (name, title, type, platform, input_types, properties, is_enable)
values ('FunAudioLLM/CosyVoice2-0.5B', '硅基流动-语音合成', 'tts', 'siliconflow', 'text', '{
  "voices": [
    {
      "name": "fnlp/MOSS-TTSD-v0.5:alex"
    },
    {
      "name": "fnlp/MOSS-TTSD-v0.5:anna"
    },
    {
      "name": "fnlp/MOSS-TTSD-v0.5:bella"
    },
    {
      "name": "fnlp/MOSS-TTSD-v0.5:benjamin"
    },
    {
      "name": "fnlp/MOSS-TTSD-v0.5:charles"
    },
    {
      "name": "fnlp/MOSS-TTSD-v0.5:claire"
    },
    {
      "name": "fnlp/MOSS-TTSD-v0.5:david"
    },
    {
      "name": "fnlp/MOSS-TTSD-v0.5:diana"
    }
  ]
}', true);
-- 预设角色
INSERT INTO adi_conversation_preset (uuid, title, remark, ai_system_message)
VALUES ('26a8f54c560948d6b2d4969f08f3f2fb', '开发工程师', '技术好', '你是一个经验丰富的开发工程师,开发技能极其熟练');
INSERT INTO adi_conversation_preset (uuid, title, remark, ai_system_message)
VALUES ('16a8f54c560949d6b2d4969f08f3f2fc', '财务专家', '算数很厉害,相关法律知识也很了解',
        '你是一个经验丰富的财务专家,精通财务分析、预算编制、财务报告、税务法规等领域知识');

-- workflow
-- 如果不定义输入的变量名，则默认设置为input
-- 如果不定义输出的变量名，则默认设置为output
insert into adi_workflow_component(uuid, name, title, remark, is_enable)
values (replace(gen_random_uuid()::text, '-', ''), 'Start', '开始', '流程由此开始', true);
insert into adi_workflow_component(uuid, name, title, remark, is_enable)
values (replace(gen_random_uuid()::text, '-', ''), 'End', '结束', '流程由此结束', true);
insert into adi_workflow_component(uuid, name, title, remark, is_enable)
values (replace(gen_random_uuid()::text, '-', ''), 'Answer', '生成回答', '调用大语言模型回答问题', true);
insert into adi_workflow_component(uuid, name, title, remark, display_order, is_enable)
values (replace(gen_random_uuid()::text, '-', ''), 'Dalle3', 'DALL-E 3 画图', '调用Dall-e-3生成图片', 11, true);
insert into adi_workflow_component(uuid, name, title, remark, display_order, is_enable)
values (replace(gen_random_uuid()::text, '-', ''), 'DocumentExtractor', '文档提取', '从文档中提取信息', 4, true);
insert into adi_workflow_component(uuid, name, title, remark, display_order, is_enable)
values (replace(gen_random_uuid()::text, '-', ''), 'KeywordExtractor', '关键词提取',
        '从内容中提取关键词，Top N指定需要提取的关键词数量', 5, true);
insert into adi_workflow_component(uuid, name, title, remark, is_enable)
values (replace(gen_random_uuid()::text, '-', ''), 'KnowledgeRetrieval', '知识检索', '从知识库中检索信息，需选中知识库',
        true);
insert into adi_workflow_component(uuid, name, title, remark, is_enable)
values (replace(gen_random_uuid()::text, '-', ''), 'Switcher', '条件分支', '根据设置的条件引导执行不同的流程', true);
insert into adi_workflow_component(uuid, name, title, remark, is_enable)
values (replace(gen_random_uuid()::text, '-', ''), 'Classifier', '内容归类',
        '使用大语言模型对输入信息进行分析并归类，根据类别调用对应的下游节点', true);
insert into adi_workflow_component(uuid, name, title, remark, display_order, is_enable)
values (replace(gen_random_uuid()::text, '-', ''), 'Template', '模板转换',
        '将多个变量合并成一个输出内容', 10, true);
insert into adi_workflow_component(uuid, name, title, remark, display_order, is_enable)
values (replace(gen_random_uuid()::text, '-', ''), 'Google', 'Google搜索', '从Google中检索信息', 13, true);
insert into adi_workflow_component(uuid, name, title, remark, display_order, is_enable)
values (replace(gen_random_uuid()::text, '-', ''), 'FaqExtractor', '常见问题提取',
        '从内容中提取出常见问题及对应的答案，Top N为提取的数量',
        6, true);
insert into adi_workflow_component(uuid, name, title, remark, display_order, is_enable)
values (replace(gen_random_uuid()::text, '-', ''), 'Tongyiwanx', '通义万相-画图', '调用文生图模型生成图片', 12, true);
insert into adi_workflow_component(uuid, name, title, remark, display_order, is_enable)
values (replace(gen_random_uuid()::text, '-', ''), 'HumanFeedback', '人机交互',
        '中断执行中的流程并等待用户的输入，用户输入后继续执行后续流程', 10, true);
insert into adi_workflow_component(uuid, name, title, remark, display_order, is_enable)
values (replace(gen_random_uuid()::text, '-', ''), 'MailSend', '邮件发送', '发送邮件到指定邮箱', 10, true);
insert into adi_workflow_component(uuid, name, title, remark, display_order, is_enable)
values (replace(gen_random_uuid()::text, '-', ''), 'HttpRequest', 'Http请求',
        '通过Http协议发送请求，可将其他组件的输出作为参数，也可设置常量作为参数。', 10, true);
-- 工作流示例
insert into adi_workflow(uuid, title, user_id, is_public, is_enable)
values ('c40cfc1792264130b1c1f82d1448648f', '中文转英文', 1, true, true);

-- 如果不定义输入的变量名，则默认设置为input
-- 如果不定义输出的变量名，则默认设置为output
INSERT INTO adi_workflow_node (uuid, workflow_id, workflow_component_id, user_id, title, input_config, node_config,
                               position_x, position_y)
VALUES ('6cbc919774aa4e779d97e3dd9c836e16', 1, 1, 0, '开始', '{
  "ref_inputs": [],
  "user_inputs": [
    {
      "name": "var_input",
      "type": 1,
      "uuid": "aed4841dfabf4e08a9cfaea98f143c08",
      "title": "翻译内容",
      "required": true
    }
  ]
}', '{}', -402.11898718515704, 325.1042741060348);
INSERT INTO adi_workflow_node (uuid, workflow_id, workflow_component_id, user_id, title, input_config, node_config,
                               position_x, position_y)
VALUES ('7bb3d7b63d2c4773aafde36fca9b67e9', 1, 3, 0, '翻译', '{
  "ref_inputs": [
    {
      "name": "var_rising",
      "node_uuid": "6cbc919774aa4e779d97e3dd9c836e16",
      "node_param_name": "var_input"
    }
  ],
  "user_inputs": []
}', '{
  "prompt": "将以下内容翻译成英文：{var_rising}",
  "model_name": "ERNIE-Speed-128K"
}', -105.91822303004142, 328.82173298096063);
INSERT INTO adi_workflow_node (uuid, workflow_id, workflow_component_id, user_id, title, input_config, node_config,
                               position_x, position_y)
VALUES ('7a71f5860723457ba5815a71cb21f4d5', 1, 9, 0, '内容分类', '{
  "ref_inputs": [],
  "user_inputs": []
}', '{
  "categories": [
    {
      "category_name": "消极的",
      "category_uuid": "f325d5e3545a42a184648b88eaaa9f3a",
      "target_node_uuid": "4892a70af36b498e89ae461e9d9a9525"
    },
    {
      "category_name": "积极的",
      "category_uuid": "caa11c60ac86409a9c0284ef5ea6a284",
      "target_node_uuid": "e87c362ff13f489dad9568f94e8219ca"
    },
    {
      "category_name": "其他的",
      "category_uuid": "ba542068bcb642f6a9287bcd0628af50",
      "target_node_uuid": "444bdaf7e35c4bb08aba35cfe42d7ee5"
    }
  ],
  "model_name": "ERNIE-Speed-128K"
}', 178.36431809503244, 256.2306178307531);
INSERT INTO adi_workflow_node (uuid, workflow_id, workflow_component_id, user_id, title, input_config, node_config,
                               position_x, position_y)
VALUES ('4892a70af36b498e89ae461e9d9a9525', 1, 8, 0, '条件分支', '{
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
VALUES ('f2b59926da754b7a8938eb3f400920ac', 1, 3, 0, '给出1个反义词', '{
  "ref_inputs": [
    {
      "name": "var_feel",
      "node_uuid": "7bb3d7b63d2c4773aafde36fca9b67e9",
      "node_param_name": "output"
    }
  ],
  "user_inputs": []
}', '{
  "prompt": "给出一个以下内容的反义词：{var_feel}",
  "model_name": "ERNIE-Speed-128K"
}', 891.5056599104687, 142.47976951620672);
INSERT INTO adi_workflow_node (uuid, workflow_id, workflow_component_id, user_id, title, input_config, node_config,
                               position_x, position_y)
VALUES ('7ac3c57446584c2b8f809daa51091eb0', 1, 3, 0, '给出10个反义词', '{
  "ref_inputs": [
    {
      "name": "var_fine",
      "node_uuid": "7bb3d7b63d2c4773aafde36fca9b67e9",
      "node_param_name": "output"
    }
  ],
  "user_inputs": []
}', '{
  "prompt": "给出10个以下内容的反义词：{var_fine},\\n##注意\\n直接列出反义词，用逗号隔开，不需要其他多余的话",
  "model_name": "ERNIE-Speed-128K"
}', 844.9517549754598, -7.5875938393296565);
INSERT INTO adi_workflow_node (uuid, workflow_id, workflow_component_id, user_id, title, input_config, node_config,
                               position_x, position_y)
VALUES ('1deb0f2df58c49768a3a5e59f825814d', 1, 10, 0, '内容模板', '{
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
  "template": "翻译结果：{var_among},10个反义词：{var_taught}"
}', 1190.5986141955661, 70.7027457958036);
INSERT INTO adi_workflow_node (uuid, workflow_id, workflow_component_id, user_id, title, input_config, node_config,
                               position_x, position_y)
VALUES ('e87c362ff13f489dad9568f94e8219ca', 1, 3, 0, '给出1个近义词', '{
  "ref_inputs": [
    {
      "name": "var_muscle",
      "node_uuid": "7bb3d7b63d2c4773aafde36fca9b67e9",
      "node_param_name": "output"
    }
  ],
  "user_inputs": []
}', '{
  "prompt": "给出一个以下内容的近义词：{var_muscle}",
  "model_name": "ERNIE-Speed-128K"
}', 575.9071882206999, 331.8698354997035);
INSERT INTO adi_workflow_node (uuid, workflow_id, workflow_component_id, user_id, title, input_config, node_config,
                               position_x, position_y)
VALUES ('444bdaf7e35c4bb08aba35cfe42d7ee5', 1, 2, 0, '结束', '{
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
  "result": "用户输入的内容：{var_again}，\n类别判断结果（使用默认输入，即上游节点的输出）：{input}，\n翻译结果（主动引入变量显示）：{var_soon}"
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

-- mcp server
-- 高德地图, 无需安装，直接使用
-- 此处preset_params中填充了高德的API密钥，preset_params表示的是系统预设的通用配置，用户无需到高德地图的网站获取key即可使用本mcp
insert into adi_mcp (uuid, title, transport_type, sse_url, sse_timeout, install_type, preset_params, website, remark,
                     is_enable)
values (replace(gen_random_uuid()::text, '-', ''), '高德地图', 'sse', 'https://mcp.amap.com/sse', 30, 'remote',
        '[
          {
            "name": "key",
            "title": "高德地图服务API密钥",
            "value": "此处填充您的高德地图API密钥（系统共用）",
            "require_encrypt": true,
            "encrypted": false
          }
        ]', 'https://lbs.amap.com/api/mcp-server/summary',
        '## 产品特点

* 使用简单：适用普通用户基于MCP（SSE）方式，不必部署本地服务，简单通过 URL 地址配置即可使用。
* 自动升级：我们会持续进行迭代更新，无须用户自己任何额外操作使用。
* 更易于大模型理解：我们对原始的JSON结果进行了语义化的转换，更易于大模型理解内容。
* 零运维成本：采用全托管云服务架构，用户无需关心服务器维护、资源扩容等底层运维问题。
* 协议兼容：支持SSE长连接，适配不同业务场景的技术需求。

## 能力介绍

* 生成专属地图
* 导航到目的地
* 打车
* 地理编码
* 逆地理编码
* IP 定位
* 天气查询
* 骑行路径规划
* 步行路径规划
* 驾车路径规划
* 公交路径规划
* 距离测量
* 关键词搜索
* 周边搜索
* 详情搜索',
        true);
-- brave search, 本地使用npx的方式安装
-- 此处customized_param_definitions中指定了用户需要填充的参数（即从Brave search获取的key），表示每个用户在使用本mcp服务前都必须填充该key以便后续MCP进行初始化及调用
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
    "value": "此处直接填充您的Brave Search API密钥",
    "encrypted": false
  }
]', true);
