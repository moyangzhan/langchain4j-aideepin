-- noinspection SqlNoDataSourceInspectionForFile

-- noinspection SqlNoDataSourceInspectionForFile

-- Install pgvector extension (https://github.com/pgvector/pgvector)
-- Install Apache AGE extension (https://github.com/apache/age)
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

-- Original table name: adi_ai_image
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
    duration              int           default 0                 not null,
    create_time           timestamp     default CURRENT_TIMESTAMP not null,
    update_time           timestamp     default CURRENT_TIMESTAMP not null,
    is_deleted            boolean       default false             not null,
    CONSTRAINT adi_draw_generate_number_check CHECK ((generate_number >= 1)),
    CONSTRAINT adi_draw_process_status_check CHECK ((process_status = ANY (ARRAY [1, 2, 3]))),
    CONSTRAINT adi_draw_user_id_check CHECK ((user_id >= 0))
);
ALTER TABLE ONLY adi_draw
    ADD CONSTRAINT udx_uuid UNIQUE (uuid);
COMMENT ON TABLE adi_draw IS 'Image generation task';
COMMENT ON COLUMN adi_draw.user_id IS 'User ID';
COMMENT ON COLUMN adi_draw.uuid IS 'UUID of the request of generated images';
COMMENT ON COLUMN adi_draw.ai_model_name IS 'Image model name';
COMMENT ON COLUMN adi_draw.prompt IS 'The prompt for generating images';
COMMENT ON COLUMN adi_draw.generate_size IS 'Image generation size';
COMMENT ON COLUMN adi_draw.generate_quality IS 'Image generation quality';
COMMENT ON COLUMN adi_draw.generate_number IS 'Number of images to generate (limited by model max_images)';
COMMENT ON COLUMN adi_draw.generate_seed IS 'Random seed for reproducible generation';
COMMENT ON COLUMN adi_draw.original_image IS 'Original image UUID (required for interacting method 2 or 3)';
COMMENT ON COLUMN adi_draw.mask_image IS 'Deprecated. The UUID of the mask image for editing (interacting method 2)';
COMMENT ON COLUMN adi_draw.resp_images_path IS 'URLs of generated images from API response, comma-separated';
COMMENT ON COLUMN adi_draw.generated_images IS 'The UUID of the generated images, separated by commas';
COMMENT ON COLUMN adi_draw.interacting_method IS 'Image generation type: 1=Text to Image, 2=Image Editing, 3=Image to Image, 4=Background Generation, 5=Style Transfer';
COMMENT ON COLUMN adi_draw.process_status IS 'Task status: 1=In progress, 2=Failed, 3=Success';
COMMENT ON COLUMN adi_draw.process_status_remark IS 'Image generation status remark';
COMMENT ON COLUMN adi_draw.is_public IS 'Whether the image is publicly visible';
COMMENT ON COLUMN adi_draw.with_watermark IS 'Whether the image has a watermark';
COMMENT ON COLUMN adi_draw.star_count IS 'Like count';
COMMENT ON COLUMN adi_draw.create_time IS 'Creation time';
COMMENT ON COLUMN adi_draw.update_time IS 'Last update time';
COMMENT ON COLUMN adi_draw.is_deleted IS 'Whether the record is soft-deleted';

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

-- In versions 3.15.0 and below, model platform data was stored in adi_sys_config
-- Manually migrate these config items: deepseek_setting, openai_setting, dashscope_setting, ollama_setting, siliconflow_setting
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

COMMENT ON TABLE adi_model_platform IS 'Model platform (model provider)';
COMMENT ON COLUMN adi_model_platform.name IS 'Model provider name, e.g., openai, dashscope, ollama';
COMMENT ON COLUMN adi_model_platform.title IS 'Model provider title, a more readable name, e.g., OpenAI, DeepSeek';
COMMENT ON COLUMN adi_model_platform.base_url IS 'API base URL of the model provider';
COMMENT ON COLUMN adi_model_platform.api_key IS 'API Key of the model provider';
COMMENT ON COLUMN adi_model_platform.secret_key IS 'Deprecated, only used by Qianfan which is no longer supported';
COMMENT ON COLUMN adi_model_platform.is_proxy_enable IS 'Whether to access the model provider API through a proxy';
COMMENT ON COLUMN adi_model_platform.is_openai_api_compatible IS 'Whether compatible with OpenAI API format';
COMMENT ON COLUMN adi_model_platform.remark IS 'Additional remarks about the model provider';

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

COMMENT ON TABLE adi_ai_model IS 'AI model';
COMMENT ON COLUMN adi_ai_model.type IS 'Model type: text, image, vision, embedding, rerank, multimodality';
COMMENT ON COLUMN adi_ai_model.name IS 'Model name as defined by the provider, must match exactly';
COMMENT ON COLUMN adi_ai_model.title IS 'Model title, a more readable name, e.g., openai-gpt3';
COMMENT ON COLUMN adi_ai_model.setting IS 'JSON format, e.g., {"voice_for_group1":"v1","voice_for_group2":"v2"}';
COMMENT ON COLUMN adi_ai_model.properties IS 'JSON, e.g., {"dimension":1536} for embedding model, {"voices":["v1","v2","v3"]} for TTS model';
COMMENT ON COLUMN adi_ai_model.remark IS 'Additional remarks about the AI model';
COMMENT ON COLUMN adi_ai_model.platform IS 'Model platform (as model provider): openai, dashscope, ollama';
COMMENT ON COLUMN adi_ai_model.context_window IS 'LLM context window';
COMMENT ON COLUMN adi_ai_model.input_types IS 'Input types: text, image, audio, video';
COMMENT ON COLUMN adi_ai_model.response_format_types IS 'Response format: text, json_object, json_schema';
COMMENT ON COLUMN adi_ai_model.is_support_web_search IS 'Whether web search is supported';
COMMENT ON COLUMN adi_ai_model.is_reasoner IS 'Whether the model is a reasoning model';
COMMENT ON COLUMN adi_ai_model.is_thinking_closable IS 'Whether the thinking process can be toggled off (e.g., Qwen3 yes, DeepSeek-R1 no)';
COMMENT ON COLUMN adi_ai_model.is_enable IS 'Whether the model is available for use';
COMMENT ON COLUMN adi_ai_model.is_free IS 'Whether the model is free to use';
COMMENT ON COLUMN adi_ai_model.create_time IS 'Creation time';
COMMENT ON COLUMN adi_ai_model.update_time IS 'Last update time';

CREATE TRIGGER trigger_ai_model_update_time
    BEFORE UPDATE
    ON adi_ai_model
    FOR EACH ROW
EXECUTE PROCEDURE update_modified_column();

-- ============================================================
-- Character: persistent personas, presets, and messages
-- ============================================================

CREATE TABLE adi_character_preset
(
    id                bigserial primary key,
    uuid              varchar(32)   default ''                not null,
    title             varchar(45)   default ''                not null,
    remark            varchar(1000) default ''                not null,
    ai_system_message varchar(1000) default ''                not null,
    kb_title          varchar(100)  default ''                not null,
    type              varchar(45)   default ''                not null,
    create_time       timestamp     default CURRENT_TIMESTAMP not null,
    update_time       timestamp     default CURRENT_TIMESTAMP not null,
    is_deleted        boolean       default false             not null
);
COMMENT ON TABLE adi_character_preset IS 'Preset Character table';
COMMENT ON COLUMN adi_character_preset.title IS 'Title';
COMMENT ON COLUMN adi_character_preset.remark IS 'Description';
COMMENT ON COLUMN adi_character_preset.ai_system_message IS 'System message for LLM';
COMMENT ON COLUMN adi_character_preset.kb_title IS 'Knowledge base title to auto-create, empty means no creation';
COMMENT ON COLUMN adi_character_preset.type IS 'Character type (technology/creative/education/business/professional/design/marketing/service/administration/utility)';

create trigger trigger_character_preset
    before update
    on adi_character_preset
    for each row
execute procedure update_modified_column();

CREATE TABLE adi_character
(
    id                        bigserial primary key,
    user_id                   bigint        default 0                 not null,
    uuid                      varchar(32)   default ''                not null,
    title                     varchar(45)   default ''                not null,
    remark                    varchar(500)  default ''                not null,
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
    api_key                   varchar(200)  default ''                not null,
    create_time               timestamp     default CURRENT_TIMESTAMP not null,
    update_time               timestamp     default CURRENT_TIMESTAMP not null,
    is_deleted                boolean       default false             not null
);

COMMENT ON TABLE adi_character IS 'Character table (represents a persistent persona with long-term memory, not a one-off session)';
COMMENT ON COLUMN adi_character.user_id IS 'User ID';
COMMENT ON COLUMN adi_character.title IS 'Title, e.g., Sherlock Holmes';
COMMENT ON COLUMN adi_character.remark IS 'Remark, e.g., Brilliant detective with keen observation skills';
COMMENT ON COLUMN adi_character.ai_system_message IS 'Character setting content, e.g., You are Sherlock Holmes, a brilliant detective known for your keen observation skills';
COMMENT ON COLUMN adi_character.llm_temperature IS 'LLM response creativity/randomness';
COMMENT ON COLUMN adi_character.mcp_ids IS 'Enabled MCP service IDs, comma-separated';
COMMENT ON COLUMN adi_character.kb_ids IS 'Associated knowledge base IDs, comma-separated';
COMMENT ON COLUMN adi_character.answer_content_type IS 'Response content type: 1=Auto, 2=Text, 3=Audio';
COMMENT ON COLUMN adi_character.is_autoplay_answer IS 'Whether audio responses play automatically';
COMMENT ON COLUMN adi_character.is_enable_thinking IS 'Whether thinking/reasoning process is enabled (effective only if the model supports it)';
COMMENT ON COLUMN adi_character.is_enable_web_search IS 'Whether to enable web search';
COMMENT ON COLUMN adi_character.audio_config IS 'Audio configuration, stored in JSON format, e.g., {"voice":{"param_name":"longyingda","model":"cosyvoice-v2","platform":"dashscope"}}';
COMMENT ON COLUMN adi_character.api_key IS 'API key for external system integration (AES encrypted)';


CREATE TRIGGER trigger_character_update_time
    BEFORE UPDATE
    ON adi_character
    FOR EACH ROW
EXECUTE PROCEDURE update_modified_column();


CREATE TABLE adi_character_preset_rel
(
    id                bigserial primary key,
    uuid              varchar(32) default ''                not null,
    user_id           bigint      default 0                 not null,
    preset_character_id bigint    default 0                 not null,
    user_character_id   bigint    default 0                 not null,
    create_time       timestamp   default CURRENT_TIMESTAMP not null,
    update_time       timestamp   default CURRENT_TIMESTAMP not null,
    is_deleted        boolean     default false             not null
);

COMMENT ON TABLE adi_character_preset_rel IS 'Preset Character Relation table';
COMMENT ON COLUMN adi_character_preset_rel.user_id IS 'User ID';
COMMENT ON COLUMN adi_character_preset_rel.preset_character_id IS 'Preset character ID';
COMMENT ON COLUMN adi_character_preset_rel.user_character_id IS 'User character ID';

create trigger trigger_character_preset_rel
    before update
    on adi_character_preset_rel
    for each row
execute procedure update_modified_column();

CREATE TABLE adi_character_message
(
    id                              bigserial primary key,
    parent_message_id               bigint        default 0                 not null,
    character_id                    bigint        default 0                 not null,
    character_uuid                  varchar(32)   default ''                not null,
    content_type                    smallint      default 2                 not null,
    remark                          text          default ''                not null,
    processed_remark                text          default ''                not null,
    thinking_content                text          default ''                not null,
    audio_uuid                      varchar(32)   default ''                not null,
    audio_duration                  integer       default 0                 not null,
    uuid                            varchar(32)   default ''                not null,
    message_role                    integer       default 1                 not null,
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

COMMENT ON TABLE adi_character_message IS 'Character Message table';
COMMENT ON COLUMN adi_character_message.parent_message_id IS 'Parent message ID';
COMMENT ON COLUMN adi_character_message.character_id IS 'Character ID';
COMMENT ON COLUMN adi_character_message.character_uuid IS 'Character UUID';
COMMENT ON COLUMN adi_character_message.remark IS 'Original message (e.g., user question, AI answer)';
COMMENT ON COLUMN adi_character_message.processed_remark IS 'Processed message (e.g., user question + knowledge base context for LLM input, or AI response after compliance filtering)';
COMMENT ON COLUMN adi_character_message.content_type IS 'Message content type: 2=Text, 3=Audio';
COMMENT ON COLUMN adi_character_message.uuid IS 'Unique identifier for the message';
COMMENT ON COLUMN adi_character_message.audio_uuid IS 'Audio file UUID (references adi_file.uuid)';
COMMENT ON COLUMN adi_character_message.audio_duration IS 'Audio duration in seconds';
COMMENT ON COLUMN adi_character_message.message_role IS 'Message role: 1=User, 2=System, 3=Assistant';
COMMENT ON COLUMN adi_character_message.user_id IS 'User ID';
COMMENT ON COLUMN adi_character_message.ai_model_id IS 'adi_ai_model id';
COMMENT ON COLUMN adi_character_message.understand_context_msg_pair_num IS 'Number of context message pairs';
COMMENT ON COLUMN adi_character_message.attachments IS 'Attachments, stored as: uuid,uuid';
COMMENT ON COLUMN adi_character_message.is_ref_embedding IS 'Whether knowledge base embeddings are referenced';
COMMENT ON COLUMN adi_character_message.is_ref_graph IS 'Whether knowledge graph is referenced';
COMMENT ON COLUMN adi_character_message.is_ref_memory_embedding IS 'Whether memory embeddings are referenced';

CREATE TRIGGER trigger_character_message_update_time
    BEFORE UPDATE
    ON adi_character_message
    FOR EACH ROW
EXECUTE PROCEDURE update_modified_column();

create table adi_character_message_ref_embedding
(
    id           bigserial primary key,
    message_id   bigint        default 0  not null,
    embedding_id varchar(36)   default '' not null,
    score        numeric(3, 2) default 0  not null,
    user_id      bigint        default 0  not null
);

comment on table adi_character_message_ref_embedding is 'Character message - knowledge base embedding references';
comment on column adi_character_message_ref_embedding.message_id is 'adi_character_message ID';
comment on column adi_character_message_ref_embedding.embedding_id is 'Embedding UUID retrieved from vector store';
comment on column adi_character_message_ref_embedding.score is 'Similarity score';
comment on column adi_character_message_ref_embedding.user_id is 'User ID';

create table adi_character_message_ref_graph
(
    id                     bigserial primary key,
    message_id             bigint default 0  not null,
    entities_from_question text   default '' not null,
    graph_from_store       text   default '' not null,
    user_id                bigint default 0  not null
);

comment on table adi_character_message_ref_graph is 'Character message - knowledge base graph references';
comment on column adi_character_message_ref_graph.message_id is 'adi_character_message ID';
comment on column adi_character_message_ref_graph.entities_from_question is 'Entities parsed from question: vertexName1,vertexName2';
comment on column adi_character_message_ref_graph.graph_from_store is 'Graph retrieved from graph database: {vertices:[{id:"111",name:"vertexName1"},{id:"222",name:"vertexName2"}],edges:[{id:"333",name:"edgeName1",start:"111",end:"222"}]';
comment on column adi_character_message_ref_graph.user_id is 'User ID';

create table adi_character_message_ref_memory_embedding
(
    id           bigserial primary key,
    message_id   bigint        default 0  not null,
    embedding_id varchar(36)   default '' not null,
    score        numeric(3, 2) default 0  not null,
    user_id      bigint        default 0  not null
);
comment on table adi_character_message_ref_memory_embedding is 'Character message - memory references';
comment on column adi_character_message_ref_memory_embedding.message_id is 'adi_character_message ID';
comment on column adi_character_message_ref_memory_embedding.embedding_id is 'Embedding UUID retrieved from memory vector store';
comment on column adi_character_message_ref_memory_embedding.score is 'Similarity score';
comment on column adi_character_message_ref_memory_embedding.user_id is 'User ID';

-- ============================================================
-- LLM Call Record: unified LLM call resource consumption tracking
-- ============================================================

CREATE TABLE adi_llm_call_record
(
    id             bigserial primary key,
    uuid           varchar(32)  not null default '',
    source_type    smallint     not null default 0,
    source_id      bigint       not null default 0,
    user_id        bigint       not null default 0,
    model_platform varchar(50)  not null default '',
    model_name     varchar(100) not null default '',
    input_tokens   integer      not null default 0,
    output_tokens  integer      not null default 0,
    duration       integer      not null default 0,
    request_time   timestamp    not null default CURRENT_TIMESTAMP,
    create_time    timestamp    not null default CURRENT_TIMESTAMP,
    update_time    timestamp    not null default CURRENT_TIMESTAMP,
    is_deleted     boolean      not null default false
);

CREATE INDEX idx_llm_call_record_source ON adi_llm_call_record(source_type, source_id);
CREATE INDEX idx_llm_call_record_user_id ON adi_llm_call_record(user_id);
CREATE INDEX idx_llm_call_record_request_time ON adi_llm_call_record(request_time);

COMMENT ON TABLE adi_llm_call_record IS 'Unified LLM call resource consumption tracking';
COMMENT ON COLUMN adi_llm_call_record.source_type IS 'Source type: 0=unknown 1=character_chat 2=knowledge_base_qa 3=knowledge_base_ingest 4=workflow_node 5=agent';
COMMENT ON COLUMN adi_llm_call_record.source_id IS 'Source record primary key';
COMMENT ON COLUMN adi_llm_call_record.user_id IS 'User ID';
COMMENT ON COLUMN adi_llm_call_record.model_platform IS 'Model platform name';
COMMENT ON COLUMN adi_llm_call_record.model_name IS 'Model name';
COMMENT ON COLUMN adi_llm_call_record.input_tokens IS 'Input token count';
COMMENT ON COLUMN adi_llm_call_record.output_tokens IS 'Output token count';
COMMENT ON COLUMN adi_llm_call_record.duration IS 'Call duration in ms';
COMMENT ON COLUMN adi_llm_call_record.request_time IS 'Request start time';

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

COMMENT ON TABLE adi_file IS 'File';
COMMENT ON COLUMN adi_file.name IS 'File name';
COMMENT ON COLUMN adi_file.uuid IS 'UUID of the file';
COMMENT ON COLUMN adi_file.ext IS 'File extension';
COMMENT ON COLUMN adi_file.user_id IS 'User ID, 0: System; Other: User';
COMMENT ON COLUMN adi_file.path IS 'File path or object name, e.g., https://*.png or 123.png (name in OSS bucket)';
COMMENT ON COLUMN adi_file.storage_location IS 'Storage Location: 1 - Local Storage, 2 - Alibaba Cloud OSS';
COMMENT ON COLUMN adi_file.ref_count IS 'The number of references to this file';
COMMENT ON COLUMN adi_file.create_time IS 'Creation time';
COMMENT ON COLUMN adi_file.update_time IS 'Last update time';
COMMENT ON COLUMN adi_file.is_deleted IS 'Whether the record is soft-deleted';
COMMENT ON COLUMN adi_file.sha256 IS 'Hash of the file';
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

COMMENT ON TABLE adi_prompt IS 'Prompt template';

COMMENT ON COLUMN adi_prompt.user_id IS 'Owner user ID (0: system)';

COMMENT ON COLUMN adi_prompt.act IS 'Prompt title';

COMMENT ON COLUMN adi_prompt.prompt IS 'Prompt content';

COMMENT ON COLUMN adi_prompt.create_time IS 'Creation time';

COMMENT ON COLUMN adi_prompt.update_time IS 'Last update time';

COMMENT ON COLUMN adi_prompt.is_deleted IS 'Whether the record is soft-deleted';

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

COMMENT ON TABLE adi_sys_config IS 'System configuration table';
COMMENT ON COLUMN adi_sys_config.name IS 'Configuration item name';
COMMENT ON COLUMN adi_sys_config.value IS 'Configuration item value';
COMMENT ON COLUMN adi_sys_config.create_time IS 'Creation time';
COMMENT ON COLUMN adi_sys_config.update_time IS 'Last update time';
COMMENT ON COLUMN adi_sys_config.is_deleted IS 'Whether the record is soft-deleted';

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

COMMENT ON TABLE adi_user IS 'User table';
COMMENT ON COLUMN adi_user.name IS 'Username';
COMMENT ON COLUMN adi_user.password IS 'Password';
COMMENT ON COLUMN adi_user.uuid IS 'UUID of the user';
COMMENT ON COLUMN adi_user.email IS 'User email';
COMMENT ON COLUMN adi_user.active_time IS 'Activation time';
COMMENT ON COLUMN adi_user.create_time IS 'Creation time';
COMMENT ON COLUMN adi_user.update_time IS 'Last update time';
COMMENT ON COLUMN adi_user.user_status IS 'User status: 1=Pending verification, 2=Active, 3=Frozen';
COMMENT ON COLUMN adi_user.is_admin IS 'Whether the user is an admin';
COMMENT ON COLUMN adi_user.is_deleted IS 'Whether the record is soft-deleted';
COMMENT ON COLUMN adi_user.quota_by_token_daily IS 'Daily token quota';
COMMENT ON COLUMN adi_user.quota_by_token_monthly IS 'Monthly token quota';
COMMENT ON COLUMN adi_user.quota_by_request_daily IS 'Daily request quota';
COMMENT ON COLUMN adi_user.quota_by_request_monthly IS 'Monthly request quota';
COMMENT ON COLUMN adi_user.understand_context_enable IS 'Context understanding switch';
COMMENT ON COLUMN adi_user.understand_context_msg_pair_num IS 'Number of context message pairs';
COMMENT ON COLUMN adi_user.quota_by_image_daily IS 'Daily image quota';
COMMENT ON COLUMN adi_user.quota_by_image_monthly IS 'Monthly image quota';

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

COMMENT ON TABLE adi_user_day_cost IS 'User daily consumption table';
COMMENT ON COLUMN adi_user_day_cost.user_id IS 'User ID';
COMMENT ON COLUMN adi_user_day_cost.day IS 'Date as an 8-digit integer, e.g., 20230901';
COMMENT ON COLUMN adi_user_day_cost.request_times IS 'Number of requests';
COMMENT ON COLUMN adi_user_day_cost.tokens IS 'Number of tokens consumed';
COMMENT ON COLUMN adi_user_day_cost.is_free IS 'Whether this row tracks free model usage (true) or paid model usage (false)';
COMMENT ON COLUMN adi_user_day_cost.create_time IS 'Creation time';
COMMENT ON COLUMN adi_user_day_cost.update_time IS 'Last update time';
COMMENT ON COLUMN adi_user_day_cost.draw_times IS 'Number of images';

CREATE TRIGGER trigger_user_day_cost_update_time
    BEFORE UPDATE
    ON adi_user_day_cost
    FOR EACH ROW
EXECUTE PROCEDURE update_modified_column();

CREATE TABLE adi_user_ext_api_key
(
    id            bigserial primary key,
    user_id       bigint        default 0                 not null,
    resource_type varchar(20)   default ''                not null,
    api_key       varchar(256)  default ''                not null,
    create_time   timestamp     default CURRENT_TIMESTAMP not null,
    update_time   timestamp     default CURRENT_TIMESTAMP not null,
    is_deleted    boolean       default false             not null
);

CREATE UNIQUE INDEX udx_user_resource_type
    ON adi_user_ext_api_key(user_id, resource_type)
    WHERE is_deleted = false;

COMMENT ON TABLE adi_user_ext_api_key IS 'User-level External API Key';
COMMENT ON COLUMN adi_user_ext_api_key.user_id IS 'User ID';
COMMENT ON COLUMN adi_user_ext_api_key.resource_type IS 'Resource type: draw, mcp';
COMMENT ON COLUMN adi_user_ext_api_key.api_key IS 'Encrypted External API Key';

CREATE TRIGGER trigger_user_ext_api_key_update_time
    BEFORE UPDATE
    ON adi_user_ext_api_key
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
    ingest_split_strategy  varchar(20)   default 'recursive'       not null,
    ingest_max_segment_size int          default 1000              not null,
    ingest_custom_separator varchar(100) default ''                not null,
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
    api_key                varchar(200)  default ''                not null,
    create_time            timestamp     default CURRENT_TIMESTAMP not null,
    update_time            timestamp     default CURRENT_TIMESTAMP not null,
    is_deleted             boolean       default false             not null
);

comment on table adi_knowledge_base is 'Knowledge Base';
comment on column adi_knowledge_base.title is 'Knowledge Base Title';
comment on column adi_knowledge_base.remark is 'Knowledge Base Description';
comment on column adi_knowledge_base.is_public is 'Is Public';
comment on column adi_knowledge_base.is_strict is 'Strict mode: return no answer if no results found in knowledge base; Non-strict: fall back to LLM if no results';
comment on column adi_knowledge_base.ingest_max_overlap is 'Max overlap (in tokens) when chunking documents, only used when cutting complete sentences';
comment on column adi_knowledge_base.ingest_split_strategy is 'Split strategy: recursive/paragraph/line/sentence/custom';
comment on column adi_knowledge_base.ingest_max_segment_size is 'Max segment size in tokens when chunking documents';
comment on column adi_knowledge_base.ingest_custom_separator is 'Custom separator for splitting, only used when strategy is custom';
comment on column adi_knowledge_base.ingest_model_name is 'LLM used for indexing/graphing documents, defaults to first available LLM';
comment on column adi_knowledge_base.ingest_model_id is 'LLM ID for indexing/graphing, defaults to first available LLM';
comment on column adi_knowledge_base.ingest_token_estimator is 'Token count estimator, default is OpenAiTokenizer';
comment on column adi_knowledge_base.ingest_embedding_model is 'Embedding model for document embedding, default is all-minilm-l6-v2';
comment on column adi_knowledge_base.retrieve_max_results is 'Max number of recall vectors, 0 means auto-adjust based on model context window';
comment on column adi_knowledge_base.retrieve_min_score is 'Min score for vector search hits, 0 means use default';
comment on column adi_knowledge_base.query_llm_temperature is 'LLM response creativity/randomness specified during user query';
COMMENT ON COLUMN adi_knowledge_base.query_system_message IS 'System message for LLM';
comment on column adi_knowledge_base.star_count is 'Number of Likes';
comment on column adi_knowledge_base.item_count is 'Number of Knowledge Items';
comment on column adi_knowledge_base.embedding_count is 'Number of embeddings';
comment on column adi_knowledge_base.owner_id is 'Owner ID';
comment on column adi_knowledge_base.owner_uuid is 'Owner UUID';
comment on column adi_knowledge_base.owner_name is 'Owner Name';
comment on column adi_knowledge_base.create_time is 'Creation time';
comment on column adi_knowledge_base.update_time is 'Last update time';
comment on column adi_knowledge_base.is_deleted is 'Whether the record is soft-deleted';
comment on column adi_knowledge_base.api_key is 'API key for external system integration (AES encrypted)';

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

comment on table adi_knowledge_base_item is 'Knowledge Base Item';
comment on column adi_knowledge_base_item.kb_id is 'Knowledge Base ID';
comment on column adi_knowledge_base_item.source_file_id is 'Source File ID';
comment on column adi_knowledge_base_item.title is 'Item Title';
comment on column adi_knowledge_base_item.brief is 'Item Brief';
comment on column adi_knowledge_base_item.remark is 'Item Content';
comment on column adi_knowledge_base_item.embedding_status is 'Embedding status: 1=Not embedded, 2=Embedding, 3=Embedded, 4=Failed';
comment on column adi_knowledge_base_item.embedding_status_change_time is 'Last embedding status change time';
comment on column adi_knowledge_base_item.graphical_status is 'Graphical status: 1=Not graphed, 2=Graphing, 3=Graphed, 4=Failed';
comment on column adi_knowledge_base_item.graphical_status_change_time is 'Last graphical status change time';
comment on column adi_knowledge_base_item.create_time is 'Creation time';
comment on column adi_knowledge_base_item.update_time is 'Last update time';
comment on column adi_knowledge_base_item.is_deleted is 'Whether the record is soft-deleted';

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

comment on table adi_knowledge_base_star is 'Knowledge Base - Like Records';
comment on column adi_knowledge_base_star.kb_id is 'adi_knowledge_base id';
comment on column adi_knowledge_base_star.kb_uuid is 'adi_knowledge_base uuid';
comment on column adi_knowledge_base_star.user_id is 'adi_user id';
comment on column adi_knowledge_base_star.user_uuid is 'adi_user uuid';
comment on column adi_knowledge_base_star.create_time is 'Creation time';
comment on column adi_knowledge_base_star.update_time is 'Last update time';
comment on column adi_knowledge_base_star.is_deleted is 'Whether the record is soft-deleted';

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
    answer          text          default ''                not null,
    source_file_ids varchar(500)  default ''                not null,
    user_id         bigint        default 0                 not null,
    ai_model_id     bigint        default 0                 not null,
    create_time     timestamp     default CURRENT_TIMESTAMP not null,
    update_time     timestamp     default CURRENT_TIMESTAMP not null,
    is_deleted      boolean       default false             not null
);

comment on table adi_knowledge_base_qa is 'Knowledge Base - Question Records';
comment on column adi_knowledge_base_qa.kb_id is 'adi_knowledge_base ID';
comment on column adi_knowledge_base_qa.kb_uuid is 'adi_knowledge_base UUID';
comment on column adi_knowledge_base_qa.question is 'User''s original question';
comment on column adi_knowledge_base_qa.prompt is 'Prompt provided to LLM';
comment on column adi_knowledge_base_qa.answer is 'Answer';
comment on column adi_knowledge_base_qa.source_file_ids is 'Source file IDs, separated by commas';
comment on column adi_knowledge_base_qa.user_id is 'User ID of the questioner';
comment on column adi_knowledge_base_qa.create_time is 'Creation time';
comment on column adi_knowledge_base_qa.update_time is 'Last update time';
comment on column adi_knowledge_base_qa.is_deleted is 'Whether the record is soft-deleted';

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

comment on table adi_knowledge_base_qa_ref_embedding is 'Knowledge Base - Question Records - Embedding References';
comment on column adi_knowledge_base_qa_ref_embedding.qa_record_id is 'adi_knowledge_base_qa ID';
comment on column adi_knowledge_base_qa_ref_embedding.embedding_id is 'adi_knowledge_base_embedding UUID';
comment on column adi_knowledge_base_qa_ref_embedding.score is 'Similarity score';
comment on column adi_knowledge_base_qa_ref_embedding.user_id is 'User ID';

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

comment on table adi_knowledge_base_graph_segment is 'Knowledge Base - Graph Segment';
comment on column adi_knowledge_base_graph_segment.uuid is 'Unique identifier';
comment on column adi_knowledge_base_graph_segment.kb_uuid is 'adi_knowledge_base UUID';
comment on column adi_knowledge_base_graph_segment.kb_item_uuid is 'adi_knowledge_base_item UUID';
comment on column adi_knowledge_base_graph_segment.remark is 'Content';
comment on column adi_knowledge_base_graph_segment.user_id is 'adi_user ID';
comment on column adi_knowledge_base_graph_segment.create_time is 'Creation time';
comment on column adi_knowledge_base_graph_segment.update_time is 'Last update time';
comment on column adi_knowledge_base_graph_segment.is_deleted is 'Whether the record is soft-deleted';

create trigger trigger_kb_graph_segment_update_time
    before update
    on adi_knowledge_base_graph_segment
    for each row
execute procedure update_modified_column();

create table adi_knowledge_base_qa_ref_graph
(
    id                     bigserial primary key,
    qa_record_id           bigint default 0  not null,
    entities_from_question text   default '' not null, -- formerly graph_from_llm
    graph_from_store       text   default '' not null,
    user_id                bigint default 0  not null
);

comment on table adi_knowledge_base_qa_ref_graph is 'Knowledge Base - Question Records - Graph References';
comment on column adi_knowledge_base_qa_ref_graph.qa_record_id is 'adi_knowledge_base_qa ID';
comment on column adi_knowledge_base_qa_ref_graph.entities_from_question is 'Graph parsed by LLM: vertexName1,vertexName2';
comment on column adi_knowledge_base_qa_ref_graph.graph_from_store is 'Graph retrieved from graph database: {vertices:[{id:"111",name:"vertexName1"},{id:"222",name:"vertexName2"}],edges:[{id:"333",name:"edgeName1",start:"111",end:"222"}]';
comment on column adi_knowledge_base_qa_ref_graph.user_id is 'adi_user ID';

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
    is_deleted    boolean      default false             not null,
    constraint uk_workflow_component_name unique (name, is_deleted)
);
create trigger trigger_workflow_component
    before update
    on adi_workflow_component
    for each row
execute procedure update_modified_column();

-- Workflow Definition (User-defined Workflow)
create table adi_workflow
(
    id          bigserial primary key,
    uuid        varchar(32)  default ''                not null,
    title       varchar(100) default ''                not null,
    remark      text         default ''                not null,
    user_id     bigint       default 0                 not null,
    is_public   boolean      default false             not null,
    is_enable   boolean      default true              not null,
    api_key     varchar(200) default ''                not null,
    create_time timestamp    default CURRENT_TIMESTAMP not null,
    update_time timestamp    default CURRENT_TIMESTAMP not null,
    is_deleted  boolean      default false             not null
);
-- definition
-- template
comment on table adi_workflow is 'Workflow Definition (User-defined Workflow)';
comment on column adi_workflow.api_key is 'API key for external system integration (AES encrypted)';
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
comment on table adi_workflow_node is 'Node of Workflow Definition';
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

-- Workflow Runtime
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
    input_tokens  int          default 0                 not null,
    output_tokens int          default 0                 not null,
    duration      int          default 0                 not null,
    create_time   timestamp    default CURRENT_TIMESTAMP not null,
    update_time   timestamp    default CURRENT_TIMESTAMP not null,
    is_deleted    boolean      default false             not null
);
COMMENT ON COLUMN adi_workflow_runtime.input IS '{"userInput01":"text01","userInput02":true,"userInput03":10,"userInput04":["selectedA","selectedB"],"userInput05":["https://a.com/a.xlxs","https://a.com/b.png"]}';
COMMENT ON COLUMN adi_workflow_runtime.status IS 'Execution status: 1=Ready, 2=In progress, 3=Success, 4=Failed';
COMMENT ON COLUMN adi_workflow_runtime.status_remark IS 'Status remark';
COMMENT ON COLUMN adi_workflow_runtime.input_tokens IS 'Total input tokens aggregated from LLM-typed nodes (terminal snapshot, written at success / fail / waiting_input)';
COMMENT ON COLUMN adi_workflow_runtime.output_tokens IS 'Total output tokens aggregated from LLM-typed nodes (terminal snapshot)';
COMMENT ON COLUMN adi_workflow_runtime.duration IS 'Total run duration in milliseconds aggregated from all nodes (terminal snapshot)';
create trigger trigger_workflow_runtime
    before update
    on adi_workflow_runtime
    for each row
execute procedure update_modified_column();

-- Workflow Runtime - Node
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
    duration            int          default 0                 not null,
    metadata            jsonb        default '{}'              not null,
    create_time         timestamp    default CURRENT_TIMESTAMP not null,
    update_time         timestamp    default CURRENT_TIMESTAMP not null,
    is_deleted          boolean      default false             not null
);
COMMENT ON COLUMN adi_workflow_runtime_node.status IS 'Execution status: 1=In progress, 2=Failed, 3=Success';
COMMENT ON COLUMN adi_workflow_runtime_node.status_remark IS 'Status remark';
create trigger trigger_workflow_runtime_node
    before update
    on adi_workflow_runtime_node
    for each row
execute procedure update_modified_column();

COMMENT ON COLUMN adi_workflow_runtime_node.duration IS 'Node execution duration in ms';
COMMENT ON COLUMN adi_workflow_runtime_node.metadata IS 'Per-node-type runtime metadata as JSON: token usage for LLM nodes, HTTP status code for HTTP nodes, search result counts for search nodes, etc.';

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
COMMENT ON TABLE adi_mcp is 'MCP server template';
COMMENT ON COLUMN adi_mcp.transport_type IS 'Transport type: 1:sse, 2:stdio';
COMMENT ON COLUMN adi_mcp.preset_params IS 'Admin-preset parameters, e.g., [{"name":"BAIDU_MAP_API_KEY","title":"Baidu Map Service","value":"111111","require_encrypt":true,"encrypted":true}]';
COMMENT ON COLUMN adi_mcp.customized_param_definitions IS 'User-configurable parameter definitions, merged with preset_params at runtime, e.g., [{"name":"GITHUB_TOKEN","title":"GitHub access token","require_encrypt":true}]';
COMMENT ON COLUMN adi_mcp.install_type IS 'Installation type: docker, local, remote, wasm';
COMMENT ON COLUMN adi_mcp.remark IS 'Supports markdown format';
COMMENT ON COLUMN adi_mcp.website IS 'Official website';

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
comment on table adi_user_mcp is 'User-enabled MCP server and configuration';
COMMENT ON COLUMN adi_user_mcp.mcp_customized_params IS 'User-defined MCP variables matching adi_mcp.customized_param_definitions, e.g., [{"name":"BAIDU_MAP_API_KEY","value":"111111","encrypted":true}]';

create trigger trigger_user_mcp
    before update
    on adi_user_mcp
    for each row
execute procedure update_modified_column();

