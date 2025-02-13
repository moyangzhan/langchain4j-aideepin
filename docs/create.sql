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
CREATE TABLE public.adi_draw
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
ALTER TABLE ONLY public.adi_draw
    ADD CONSTRAINT udx_uuid UNIQUE (uuid);
COMMENT ON TABLE public.adi_draw IS '绘图任务 | Image generation task';
COMMENT ON COLUMN public.adi_draw.user_id IS '用户ID | User ID';
COMMENT ON COLUMN public.adi_draw.uuid IS '绘图任务的UUID | UUID of the request of generated images';
COMMENT ON COLUMN public.adi_draw.ai_model_name IS '图像模型名称 | Image model name';
COMMENT ON COLUMN public.adi_draw.prompt IS '生成图片的提示词 | The prompt for generating images';
COMMENT ON COLUMN public.adi_draw.generate_size IS '生成图片的尺寸 | Image generation size';
COMMENT ON COLUMN public.adi_draw.generate_quality IS '生成图片的质量 | Image generation quality';
COMMENT ON COLUMN public.adi_draw.generate_number IS '生成图片的数量，必须在1到10之间，默认为1 | The number of images to generate. Must be between 1 and 10. Defaults to 1.';
COMMENT ON COLUMN public.adi_draw.generate_seed IS '生成图片的随机种子，如果希望生成内容保持相对稳定，请使用相同的seed参数值 | The random seed of the generated image. If you want the generated content to remain relatively stable, use the same seed parameter value';
COMMENT ON COLUMN public.adi_draw.original_image IS '原始图片的UUID，交互方式必须为2或3 | The UUID of the original image, interacting method must be 2 or 3';
COMMENT ON COLUMN public.adi_draw.mask_image IS '遮罩图片的UUID，交互方式必须为2 | The UUID of the mask image, interacting method must be 2';
COMMENT ON COLUMN public.adi_draw.resp_images_path IS '从OpenAI响应生成图片的URL，逗号隔开 | The URL of the generated images from OpenAI response, separated by commas';
COMMENT ON COLUMN public.adi_draw.generated_images IS '生成的多张图片文件UUID，逗号隔开 | The UUID of the generated images, separated by commas';
COMMENT ON COLUMN public.adi_draw.interacting_method IS '交互方式：1：文本生成图片；2：图片编辑；3：图片生成图片；4：背景生成；5：扩大图片；6：风格转化 | Image generation task type: 1: Text to Image; 2: Image Editing; 3: Image to Image; 4: Background Generation; 5: Style Transfer';
COMMENT ON COLUMN public.adi_draw.process_status IS '任务执行状态，1：进行中，2：失败，3：成功 | Task execution status, 1: In progress, 2: Failed, 3: Success';
COMMENT ON COLUMN public.adi_draw.process_status_remark IS '生成图片状态备注 | Image generation status remark';
COMMENT ON COLUMN public.adi_draw.is_public IS '是否公开 | Is public';
COMMENT ON COLUMN public.adi_draw.with_watermark IS '是否带水印 | With watermark';
COMMENT ON COLUMN public.adi_draw.star_count IS '点赞数 | Number of Likes';
COMMENT ON COLUMN public.adi_draw.create_time IS '记录创建的时间戳 | Timestamp of record creation';
COMMENT ON COLUMN public.adi_draw.update_time IS '记录最后更新的时间戳，自动更新 | Timestamp of record last update, automatically updated on each update';
COMMENT ON COLUMN public.adi_draw.is_deleted IS '记录是否被删除的标志（0：未删除，1：已删除） | Flag indicating whether the record is deleted (0: not deleted, 1: deleted)';

CREATE TRIGGER trigger_draw_update_time
    BEFORE UPDATE
    ON adi_draw
    FOR EACH ROW
EXECUTE PROCEDURE update_modified_column();

CREATE TABLE public.adi_draw_star
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

CREATE TABLE public.adi_draw_comment
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

CREATE TABLE public.adi_ai_model
(
    id                bigserial primary key,
    name              varchar(45)   default ''                not null,
    title             varchar(45)   default ''                not null,
    type              varchar(45)   default 'text'            not null,
    setting           varchar(500)  default ''                not null,
    remark            varchar(1000) default '',
    platform          varchar(45)   default ''                not null,
    context_window    int           default 0                 not null,
    max_input_tokens  int           default 0                 not null,
    max_output_tokens int           default 0                 not null,
    input_types       varchar(100)  default 'text'            not null,
    is_free           boolean       default false             not null,
    is_enable         boolean       default false             not null,
    create_time       timestamp     default CURRENT_TIMESTAMP not null,
    update_time       timestamp     default CURRENT_TIMESTAMP not null,
    is_deleted        boolean       default false             not null
);

COMMENT ON TABLE public.adi_ai_model IS 'AI模型 | AI model';
COMMENT ON COLUMN public.adi_ai_model.type IS '模型类型(以输出类型或使用目的做判断，如dalle2可文本和图像输入，但使用方关注的是输出的图片，所以属于image类型),eg: text,image,embedding,rerank | Model type (judged by output type or usage purpose, e.g., dalle2 can input text and image, but users focus on the output image, so it belongs to the image type), e.g., text, image, embedding, rerank';
COMMENT ON COLUMN public.adi_ai_model.name IS '模型名称 | Model name';
COMMENT ON COLUMN public.adi_ai_model.remark IS '备注 | Additional remarks about the AI model';
COMMENT ON COLUMN public.adi_ai_model.platform IS '平台 | Platform, e.g., openai, dashscope, qianfan, ollama';
COMMENT ON COLUMN public.adi_ai_model.context_window IS '上下文窗口 | LLM context window';
COMMENT ON COLUMN public.adi_ai_model.input_types IS '输入类型 | Input types, e.g., text, image, audio, video';
COMMENT ON COLUMN public.adi_ai_model.is_enable IS '是否启用 | True: Normal usage, false: Not available';
COMMENT ON COLUMN public.adi_ai_model.is_free IS '是否免费 | Is free, true: free, false: paid';
COMMENT ON COLUMN public.adi_ai_model.create_time IS '创建时间 | Timestamp of record creation';
COMMENT ON COLUMN public.adi_ai_model.update_time IS '更新时间 | Timestamp of record last update, automatically updated on each update';

CREATE TRIGGER trigger_ai_model_update_time
    BEFORE UPDATE
    ON adi_ai_model
    FOR EACH ROW
EXECUTE PROCEDURE update_modified_column();

CREATE TABLE public.adi_conversation_preset
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
COMMENT ON TABLE public.adi_conversation_preset IS '预设会话(角色)表 | Preset conversation (role) table';
COMMENT ON COLUMN public.adi_conversation_preset.title IS '标题 | Title';
COMMENT ON COLUMN public.adi_conversation_preset.remark IS '描述 | Description';
COMMENT ON COLUMN public.adi_conversation_preset.ai_system_message IS '提供给LLM的系统信息 | System message for LLM';

create trigger trigger_conversation_preset
    before update
    on adi_conversation_preset
    for each row
execute procedure update_modified_column();

CREATE TABLE public.adi_conversation
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
    create_time               timestamp     default CURRENT_TIMESTAMP not null,
    update_time               timestamp     default CURRENT_TIMESTAMP not null,
    is_deleted                boolean       default false             not null
);

COMMENT ON TABLE public.adi_conversation IS '用户会话(角色)表 | User conversation (role) table';
COMMENT ON COLUMN public.adi_conversation.user_id IS '用户ID | User ID';
COMMENT ON COLUMN public.adi_conversation.title IS '标题，如：狄仁杰 | Title, e.g., Sherlock Holmes';
COMMENT ON COLUMN public.adi_conversation.remark IS '备注，如：断案如神，手下能人众多 | Remark, e.g., Brilliant detective with keen observation skills';
COMMENT ON COLUMN public.adi_conversation.ai_system_message IS '角色设定内容，如：你是唐朝的狄仁杰，破了很多大案、疑案 | Role setting content, e.g., You are Sherlock Holmes, a brilliant detective known for your keen observation skills';
COMMENT ON COLUMN public.adi_conversation.llm_temperature IS 'LLM响应的创造性/随机性 | LLM response creativity/randomness';

CREATE TRIGGER trigger_conv_update_time
    BEFORE UPDATE
    ON adi_conversation
    FOR EACH ROW
EXECUTE PROCEDURE update_modified_column();


CREATE TABLE public.adi_conversation_preset_rel
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

COMMENT ON TABLE public.adi_conversation_preset_rel IS '预设会话与用户会话关系表 | Preset conversation and user conversation relationship table';
COMMENT ON COLUMN public.adi_conversation_preset_rel.user_id IS '用户ID | User ID';
COMMENT ON COLUMN public.adi_conversation_preset_rel.preset_conv_id IS '预设会话ID | Preset conversation ID';
COMMENT ON COLUMN public.adi_conversation_preset_rel.user_conv_id IS '用户会话ID | User conversation ID';

create trigger trigger_conversation_preset_rel
    before update
    on adi_conversation_preset_rel
    for each row
execute procedure update_modified_column();

CREATE TABLE public.adi_conversation_message
(
    id                              bigserial primary key,
    parent_message_id               bigint        default 0                 not null,
    conversation_id                 bigint        default 0                 not null,
    conversation_uuid               varchar(32)   default '',
    remark                          text                                    not null,
    uuid                            varchar(32)   default '',
    message_role                    integer       default 1                 not null,
    tokens                          integer       default 0                 not null,
    user_id                         bigint        default 0                 not null,
    ai_model_id                     bigint        default 0                 not null,
    understand_context_msg_pair_num integer       default 0                 not null,
    attachments                     varchar(1000) default ''                not null,
    create_time                     timestamp     default CURRENT_TIMESTAMP not null,
    update_time                     timestamp     default CURRENT_TIMESTAMP not null,
    is_deleted                      boolean       default false             not null
);

COMMENT ON TABLE public.adi_conversation_message IS '对话消息表 | Conversation message table';
COMMENT ON COLUMN public.adi_conversation_message.parent_message_id IS '父级消息id | Parent message ID';
COMMENT ON COLUMN public.adi_conversation_message.conversation_id IS '对话id | Conversation ID';
COMMENT ON COLUMN public.adi_conversation_message.conversation_uuid IS '对话的UUID | Conversation UUID';
COMMENT ON COLUMN public.adi_conversation_message.remark IS 'AI回复的消息 | AI response message';
COMMENT ON COLUMN public.adi_conversation_message.uuid IS '唯一标识消息的UUID | Unique identifier for the message';
COMMENT ON COLUMN public.adi_conversation_message.message_role IS '产生该消息的角色：1: 用户, 2: 系统, 3: 助手 | Role that generated the message: 1: User, 2: System, 3: Assistant';
COMMENT ON COLUMN public.adi_conversation_message.tokens IS '消耗的token数量 | Number of tokens consumed';
COMMENT ON COLUMN public.adi_conversation_message.user_id IS '用户ID | User ID';
COMMENT ON COLUMN public.adi_conversation_message.ai_model_id IS '模型表的ID | adi_ai_model id';
COMMENT ON COLUMN public.adi_conversation_message.understand_context_msg_pair_num IS '上下文消息对数量 | Number of context message pairs';
COMMENT ON COLUMN public.adi_conversation_message.attachments IS '附件,存储格式: uuid,uuid | Attachments, stored as: uuid,uuid';

CREATE TRIGGER trigger_conv_message_update_time
    BEFORE UPDATE
    ON adi_conversation_message
    FOR EACH ROW
EXECUTE PROCEDURE update_modified_column();

CREATE TABLE public.adi_file
(
    id               bigserial primary key,
    name             varchar(36)  default ''                not null,
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

COMMENT ON TABLE public.adi_file IS '文件 | File';
COMMENT ON COLUMN public.adi_file.name IS '文件名 | File name';
COMMENT ON COLUMN public.adi_file.uuid IS '文件的UUID | UUID of the file';
COMMENT ON COLUMN public.adi_file.ext IS '文件扩展名 | File extension';
COMMENT ON COLUMN public.adi_file.user_id IS '用户ID，0: 系统；其他: 用户 | User ID, 0: System; Other: User';
COMMENT ON COLUMN public.adi_file.path IS '文件路径或对象名称(OSS)，如https://*.png 或 123.png | File path or object name, eg: httts://*.png or 123.png(name in OSS bucket)';
COMMENT ON COLUMN public.adi_file.storage_location IS '存储位置，1：本地存储，2：阿里云OSS | Storage Location: 1 - Local Storage, 2 - Alibaba Cloud OSS';
COMMENT ON COLUMN public.adi_file.ref_count IS '引用此文件的次数 | The number of references to this file';
COMMENT ON COLUMN public.adi_file.create_time IS '记录创建的时间戳 | Timestamp of record creation';
COMMENT ON COLUMN public.adi_file.update_time IS '记录最后更新的时间戳，自动更新 | Timestamp of record last update, automatically updated on each update';
COMMENT ON COLUMN public.adi_file.is_deleted IS '是否删除，0: 正常；1: 删除 | Deletion status, 0: Normal; 1: Deleted';
COMMENT ON COLUMN public.adi_file.sha256 IS '文件的哈希值 | Hash of the file';
CREATE TRIGGER trigger_file_update_time
    BEFORE UPDATE
    ON adi_file
    FOR EACH ROW
EXECUTE PROCEDURE update_modified_column();

CREATE TABLE public.adi_prompt
(
    id          bigserial primary key,
    user_id     bigint                 default 0                 not null,
    act         character varying(120) default ''                not null,
    prompt      text                                             not null,
    create_time timestamp              default CURRENT_TIMESTAMP not null,
    update_time timestamp              default CURRENT_TIMESTAMP not null,
    is_deleted  boolean                default false             not null
);

COMMENT ON TABLE public.adi_prompt IS '提示词';

COMMENT ON COLUMN public.adi_prompt.user_id IS '所属用户(0: system)';

COMMENT ON COLUMN public.adi_prompt.act IS '提示词标题';

COMMENT ON COLUMN public.adi_prompt.prompt IS '提示词内容';

COMMENT ON COLUMN public.adi_prompt.create_time IS 'Timestamp of record creation';

COMMENT ON COLUMN public.adi_prompt.update_time IS 'Timestamp of record last update, automatically updated on each update';

COMMENT ON COLUMN public.adi_prompt.is_deleted IS '0:未删除；1：已删除';

CREATE TRIGGER trigger_prompt_update_time
    BEFORE UPDATE
    ON adi_prompt
    FOR EACH ROW
EXECUTE PROCEDURE update_modified_column();

CREATE TABLE public.adi_sys_config
(
    id          bigserial primary key,
    name        character varying(100)  default ''             not null,
    value       character varying(1000) default ''             not null,
    create_time timestamp               default localtimestamp not null,
    update_time timestamp               default localtimestamp not null,
    is_deleted  boolean                 default false          not null
);

COMMENT ON TABLE public.adi_sys_config IS '系统配置表 | System configuration table';
COMMENT ON COLUMN public.adi_sys_config.name IS '配置项名称 | Configuration item name';
COMMENT ON COLUMN public.adi_sys_config.value IS '配置项值 | Configuration item value';
COMMENT ON COLUMN public.adi_sys_config.create_time IS '记录创建的时间戳 | Timestamp of record creation';
COMMENT ON COLUMN public.adi_sys_config.update_time IS '记录最后更新的时间戳，自动更新 | Timestamp of record last update, automatically updated on each update';
COMMENT ON COLUMN public.adi_sys_config.is_deleted IS '是否删除，0：未删除；1：已删除 | Deletion status, 0: Not deleted; 1: Deleted';

CREATE TRIGGER trigger_sys_config_update_time
    BEFORE UPDATE
    ON adi_sys_config
    FOR EACH ROW
EXECUTE PROCEDURE update_modified_column();

CREATE TABLE public.adi_user
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

COMMENT ON TABLE public.adi_user IS '用户表 | User table';
COMMENT ON COLUMN public.adi_user.name IS '用户名 | Username';
COMMENT ON COLUMN public.adi_user.password IS '密码 | Password';
COMMENT ON COLUMN public.adi_user.uuid IS '用户的UUID | UUID of the user';
COMMENT ON COLUMN public.adi_user.email IS '用户邮箱 | User email';
COMMENT ON COLUMN public.adi_user.active_time IS '激活时间 | Activation time';
COMMENT ON COLUMN public.adi_user.create_time IS '记录创建的时间戳 | Timestamp of record creation';
COMMENT ON COLUMN public.adi_user.update_time IS '记录最后更新的时间戳，自动更新 | Timestamp of record last update, automatically updated on each update';
COMMENT ON COLUMN public.adi_user.user_status IS '用户状态，1：待验证；2：正常；3：冻结 | User status, 1: Pending verification; 2: Active; 3: Frozen';
COMMENT ON COLUMN public.adi_user.is_admin IS '是否管理员，0：否；1：是 | Is admin, 0: No; 1: Yes';
COMMENT ON COLUMN public.adi_user.is_deleted IS '是否删除，0：未删除；1：已删除 | Deletion status, 0: Not deleted; 1: Deleted';
COMMENT ON COLUMN public.adi_user.quota_by_token_daily IS '每日token配额 | Daily token quota';
COMMENT ON COLUMN public.adi_user.quota_by_token_monthly IS '每月token配额 | Monthly token quota';
COMMENT ON COLUMN public.adi_user.quota_by_request_daily IS '每日请求配额 | Daily request quota';
COMMENT ON COLUMN public.adi_user.quota_by_request_monthly IS '每月请求配额 | Monthly request quota';
COMMENT ON COLUMN public.adi_user.understand_context_enable IS '上下文理解开关 | Context understanding switch';
COMMENT ON COLUMN public.adi_user.understand_context_msg_pair_num IS '上下文消息对数量 | Number of context message pairs';
COMMENT ON COLUMN public.adi_user.quota_by_image_daily IS '每日图片配额 | Daily image quota';
COMMENT ON COLUMN public.adi_user.quota_by_image_monthly IS '每月图片配额 | Monthly image quota';

CREATE TRIGGER trigger_user_update_time
    BEFORE UPDATE
    ON adi_user
    FOR EACH ROW
EXECUTE PROCEDURE update_modified_column();

CREATE TABLE public.adi_user_day_cost
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

COMMENT ON TABLE public.adi_user_day_cost IS '用户每天消耗总量表 | User daily consumption table';
COMMENT ON COLUMN public.adi_user_day_cost.user_id IS '用户ID | User ID';
COMMENT ON COLUMN public.adi_user_day_cost.day IS '日期，用7位整数表示，如20230901 | Date, represented as a 7-digit integer, e.g., 20230901';
COMMENT ON COLUMN public.adi_user_day_cost.request_times IS '请求数量 | Number of requests';
COMMENT ON COLUMN public.adi_user_day_cost.tokens IS '消耗的token数量 | Number of tokens consumed';
COMMENT ON COLUMN public.adi_user_day_cost.is_free IS '是：免费额度(即该行统计的是免费模型消耗的额度)；否：收费额度(即该行统计的是收费模型消耗的额度) | Yes: Free quota (the row counts the consumption of free models); No: Paid quota (the row counts the consumption of paid models)';
COMMENT ON COLUMN public.adi_user_day_cost.create_time IS '记录创建的时间戳 | Timestamp of record creation';
COMMENT ON COLUMN public.adi_user_day_cost.update_time IS '记录最后更新的时间戳，自动更新 | Timestamp of record last update, automatically updated on each update';
COMMENT ON COLUMN public.adi_user_day_cost.draw_times IS '图片数量 | Number of images';

CREATE TRIGGER trigger_user_day_cost_update_time
    BEFORE UPDATE
    ON adi_user_day_cost
    FOR EACH ROW
EXECUTE PROCEDURE update_modified_column();

create table adi_knowledge_base
(
    id                    bigserial primary key,
    uuid                  varchar(32)   default ''                not null,
    title                 varchar(250)  default ''                not null,
    remark                text          default ''                not null,
    is_public             boolean       default false             not null,
    is_strict             boolean       default true              not null,
    ingest_max_overlap    int           default 0                 not null,
    ingest_model_name     varchar(45)   default ''                not null,
    ingest_model_id       bigint        default 0                 not null,
    retrieve_max_results  int           default 3                 not null,
    retrieve_min_score    numeric(2, 1) default 0.6               not null,
    query_llm_temperature numeric(2, 1) default 0.7               not null,
    owner_id              bigint        default 0                 not null,
    owner_uuid            varchar(32)   default ''                not null,
    owner_name            varchar(45)   default ''                not null,
    star_count            int           default 0                 not null,
    item_count            int           default 0                 not null,
    embedding_count       int           default 0                 not null,
    create_time           timestamp     default CURRENT_TIMESTAMP not null,
    update_time           timestamp     default CURRENT_TIMESTAMP not null,
    is_deleted            boolean       default false             not null
);

comment on table adi_knowledge_base is '知识库 | Knowledge Base';
comment on column adi_knowledge_base.title is '知识库名称 | Knowledge Base Title';
comment on column adi_knowledge_base.remark is '知识库描述 | Knowledge Base Description';
comment on column adi_knowledge_base.is_public is '是否公开 | Is Public';
comment on column adi_knowledge_base.is_strict is '是否严格模式,严格模式：严格匹配知识库，知识库中如无搜索结果，直接返回无答案;非严格模式：非严格匹配知识库，知识库中如无搜索结果，将用户提问传给LLM继续请求答案 | Is Strict Mode: Strict mode strictly matches the knowledge base, if there are no search results in the knowledge base, it directly returns no answer; Non-strict mode: Non-strictly matches the knowledge base, if there are no search results in the knowledge base, the question is passed to the LLM for further answers';
comment on column adi_knowledge_base.ingest_max_overlap is '设置文档切块时重叠的最大数量（按token来计），对完整句子切割时才考虑重叠 | Maximum overlap when chunking documents (measured in tokens), only considered when cutting complete sentences';
comment on column adi_knowledge_base.ingest_model_name is '索引(图谱化)文档时使用的LLM,不指定时使用第1个可用的LLM | LLM used when indexing (graphing) documents, if not specified, the first available LLM is used';
comment on column adi_knowledge_base.ingest_model_id is '索引(图谱化)文档时使用的LLM,不指定时使用第1个可用的LLM | LLM ID used when indexing (graphing) documents, if not specified, the first available LLM is used';
comment on column adi_knowledge_base.retrieve_max_results is '设置召回向量最大数量,默认为0,表示由系统根据模型的contentWindow自动调整 | Set the maximum number of recall vectors, default is 0, meaning the system automatically adjusts based on the model''s content window';
comment on column adi_knowledge_base.retrieve_min_score is '设置向量搜索时命中所需的最低分数,为0表示使用默认 | Set the minimum score required for a hit in vector search, 0 means using the default';
comment on column adi_knowledge_base.query_llm_temperature is '用户查询时指定LLM响应时的创造性/随机性 | LLM response creativity/randomness specified during user query';
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
comment on column adi_knowledge_base_qa_ref_embedding.embedding_id is '向量uuid | adi_knowledge_base_embedding UUID';
comment on column adi_knowledge_base_qa_ref_embedding.score is '评分 | Score';
comment on column adi_knowledge_base_qa_ref_embedding.user_id is '所属用户 | User ID';

create trigger trigger_kb_qa_ref_update_time
    before update
    on adi_knowledge_base_qa_ref_embedding
    for each row
execute procedure update_modified_column();

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
    id               bigserial primary key,
    qa_record_id     bigint default 0  not null,
    graph_from_llm   text   default '' not null,
    graph_from_store text   default '' not null,
    user_id          bigint default 0  not null
);

comment on table adi_knowledge_base_qa_ref_graph is '知识库-提问记录-图谱引用记录 | Knowledge Base - Question Records - Graph References';
comment on column adi_knowledge_base_qa_ref_graph.qa_record_id is '提问记录id | adi_knowledge_base_qa ID';
comment on column adi_knowledge_base_qa_ref_graph.graph_from_llm is 'LLM解析出来的图谱: vertexName1,vertexName2 | Graph parsed by LLM: vertexName1,vertexName2';
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

-- 初始化数据 --

-- 管理员账号：catkeeper@aideepin.com  密码：123456
INSERT INTO adi_user (name, password, uuid, email, user_status, is_admin)
VALUES ('catkeeper', '$2a$10$z44gncmQk6xCBCeDx55gMe1Zc8uYtOKcoT4/HE2F92VcF7wP2iquG',
        replace(gen_random_uuid()::text, '-', ''), 'catkeeper@aideepin.com', 2, true);

-- 配置信息
INSERT INTO adi_sys_config (name, value)
VALUES ('openai_setting', '{"secret_key":""}');
INSERT INTO adi_sys_config (name, value)
VALUES ('dashscope_setting', '{"api_key":""}');
INSERT INTO adi_sys_config (name, value)
VALUES ('qianfan_setting', '{"api_key":"","secret_key":""}');
INSERT INTO adi_sys_config (name, value)
VALUES ('ollama_setting', '{"base_url":""}');
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

-- 大语言模型
-- https://platform.openai.com/docs/models/gpt-3-5-turbo
INSERT INTO adi_ai_model (name, title, type, platform, context_window, max_input_tokens, max_output_tokens, is_enable)
VALUES ('gpt-3.5-turbo', 'gpt3.5', 'text', 'openai', 16385, 12385, 4096, false);
INSERT INTO adi_ai_model (name, title, type, platform, is_enable)
VALUES ('dall-e-2', 'DALL-E-2', 'image', 'openai', false);
INSERT INTO adi_ai_model (name, title, type, platform, is_enable)
VALUES ('dall-e-3', 'DALL-E-3', 'image', 'openai', false);
-- https://help.aliyun.com/zh/dashscope/developer-reference/model-introduction?spm=a2c4g.11186623.0.i39
INSERT INTO adi_ai_model (name, title, type, platform, context_window, max_input_tokens, max_output_tokens, is_enable)
VALUES ('qwen-turbo', '通义千问turbo', 'text', 'dashscope', 8192, 6144, 1536, false);
-- 图片识别
INSERT INTO adi_ai_model (name, title, type, platform, context_window, max_input_tokens, max_output_tokens, input_types,
                          is_enable)
VALUES ('qwen2-vl-7b-instruct', '通义千问-识图', 'text', 'dashscope', 32768, 16384, 16384, 'text,image', false);
-- https://help.aliyun.com/zh/model-studio/developer-reference/text-to-image-v2-api-reference?spm=a2c4g.11186623.0.i2
-- 通义万相-文生图（wanx2.1-t2i-plus、wanx2.1-t2i-turbo）
INSERT INTO adi_ai_model (name, title, type, platform, is_enable)
VALUES ('wanx2.1-t2i-turbo', '通义万相-文生图', 'image', 'dashscope', false);
-- 通义万相-切换背景
INSERT INTO adi_ai_model (name, title, type, platform, is_enable)
VALUES ('wanx-background-generation-v2', '通义万相-背景生成', 'image', 'dashscope', false);
-- https://console.bce.baidu.com/qianfan/modelcenter/model/buildIn/detail/am-bg7n2rn2gsbb
INSERT INTO adi_ai_model (name, title, type, platform, context_window, max_input_tokens, max_output_tokens, is_free,
                          is_enable,
                          setting)
VALUES ('ERNIE-Speed-128K', 'ERNIE-Speed-128K', 'text', 'qianfan', 131072, 126976, 4096, true, false,
        '{"endpoint":"ernie-speed-128k"}');
INSERT INTO adi_ai_model (name, title, type, platform, is_enable)
VALUES ('tinydolphin', 'ollama-tinydolphin', 'text', 'ollama', false);

-- 预设角色
INSERT INTO adi_conversation_preset (uuid, title, remark, ai_system_message)
VALUES ('26a8f54c560948d6b2d4969f08f3f2fb', '开发工程师', '技术好', '你是一个经验丰富的开发工程师,开发技能极其熟练');
INSERT INTO adi_conversation_preset (uuid, title, remark, ai_system_message)
VALUES ('16a8f54c560949d6b2d4969f08f3f2fc', '财务专家', '算数很厉害,相关法律知识也很了解',
        '你是一个经验丰富的财务专家,精通财务分析、预算编制、财务报告、税务法规等领域知识');