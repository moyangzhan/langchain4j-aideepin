CREATE TABLE `adi_ai_model`
(
    `id`           bigint      NOT NULL AUTO_INCREMENT,
    `name`         varchar(45) NOT NULL DEFAULT '',
    `remark`       varchar(1000)        DEFAULT NULL,
    `model_status` tinyint     NOT NULL DEFAULT '1' COMMENT '1:正常使用,2:不可用',
    `create_time`  datetime    NOT NULL DEFAULT CURRENT_TIMESTAMP,
    `update_time`  datetime    NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    PRIMARY KEY (`id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4
  COLLATE = utf8mb4_general_ci COMMENT ='ai模型';

CREATE TABLE `adi_sys_config`
(
    `id`          bigint       NOT NULL AUTO_INCREMENT,
    `name`        varchar(100) NOT NULL DEFAULT '',
    `value`       varchar(100) NOT NULL DEFAULT '',
    `create_time` datetime     NOT NULL DEFAULT CURRENT_TIMESTAMP,
    `update_time` datetime     NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    `is_delete`   tinyint      NOT NULL DEFAULT '0',
    PRIMARY KEY (`id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4
  COLLATE = utf8mb4_general_ci COMMENT ='系统配置表';

INSERT INTO `adi_sys_config` (`name`, `value`)
VALUES ('secret_key', '');
INSERT INTO `adi_sys_config` (`name`, `value`)
VALUES ('request_text_rate_limit', '{"times":24,"minutes":3}');
INSERT INTO `adi_sys_config` (`name`, `value`)
VALUES ('request_image_rate_limit', '{"times":6,"minutes":3}');
INSERT INTO `adi_sys_config` (`name`, `value`)
VALUES ('conversation_max_num', '50');
INSERT INTO `adi_sys_config` (`name`, `value`)
VALUES ('quota_by_token_daily', '10000');
INSERT INTO `adi_sys_config` (`name`, `value`)
VALUES ('quota_by_token_monthly', '200000');
INSERT INTO `adi_sys_config` (`name`, `value`)
VALUES ('quota_by_request_daily', '150');
INSERT INTO `adi_sys_config` (`name`, `value`)
VALUES ('quota_by_request_monthly', '3000');
INSERT INTO `adi_sys_config` (`name`, `value`)
VALUES ('quota_by_image_daily', '30');
INSERT INTO `adi_sys_config` (`name`, `value`)
VALUES ('quota_by_image_monthly', '300');

CREATE TABLE `adi_conversation`
(
    `id`                        bigint        NOT NULL AUTO_INCREMENT,
    `user_id`                   bigint        NOT NULL DEFAULT '0' COMMENT '用户id',
    `title`                     varchar(45)   NOT NULL DEFAULT '' COMMENT '对话标题',
    `uuid`                      varchar(32)   NOT NULL DEFAULT '',
    `understand_context_enable` tinyint       NOT NULL default '0' COMMENT '是否开启上下文理解',
    `ai_model`                  varchar(45)   NOT NULL DEFAULT '' COMMENT 'ai model',
    `ai_system_message`         varchar(1024) NOT NULL DEFAULT '' COMMENT 'set the system message to ai, ig: you are a lawyer',
    `tokens`                    int           NOT NULL DEFAULT '0' COMMENT '消耗token数量',
    `create_time`               datetime      NOT NULL DEFAULT CURRENT_TIMESTAMP,
    `update_time`               datetime      NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    `is_delete`                 tinyint       NOT NULL DEFAULT '0',
    PRIMARY KEY (`id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4
  COLLATE = utf8mb4_general_ci COMMENT ='对话表';

CREATE TABLE `adi_conversation_message`
(
    `id`                              bigint      NOT NULL AUTO_INCREMENT,
    `parent_message_id`               bigint      NOT NULL DEFAULT '0' COMMENT '父级消息id',
    `conversation_id`                 bigint      NOT NULL DEFAULT '0',
    `conversation_uuid`               varchar(32) NOT NULL DEFAULT '',
    `user_id`                         bigint      NOT NULL DEFAULT '0' COMMENT 'User id',
    `content`                         text        NOT NULL COMMENT '对话的消息',
    `uuid`                            varchar(32) NOT NULL DEFAULT '',
    `message_role`                    varchar(25) NOT NULL DEFAULT '' COMMENT '产生该消息的角色：1:user,2:system,3:assistant',
    `tokens`                          int         NOT NULL DEFAULT '0' COMMENT '消耗的token数量',
    `secret_key_type`                 int         NOT NULL DEFAULT '1' COMMENT '1:System secret key,2:User secret key',
    `understand_context_msg_pair_num` int         NOT NULL DEFAULT '0' COMMENT 'If context understanding enable, context_pair_msg_num > 0',
    `create_time`                     datetime    NOT NULL DEFAULT CURRENT_TIMESTAMP,
    `update_time`                     datetime    NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    `is_delete`                       tinyint     NOT NULL DEFAULT '0' COMMENT '是否删除,0:未删除,1:已删除',
    PRIMARY KEY (`id`),
    UNIQUE KEY `udx_uuid` (`uuid`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4
  COLLATE = utf8mb4_general_ci COMMENT ='对话信息表';

CREATE TABLE `adi_ai_image`
(
    `id`                 bigint        NOT NULL AUTO_INCREMENT,
    `user_id`            bigint        NOT NULL DEFAULT '0' COMMENT 'The user who generated the image',
    `uuid`               varchar(32)   NOT NULL DEFAULT '' COMMENT 'The uuid of the request of generated images',
    `prompt`             varchar(1024) NOT NULL DEFAULT '' COMMENT 'The prompt for generating images',
    `generate_size`      varchar(20)   NOT NULL DEFAULT '' COMMENT 'The size of the generated images. Must be one of "256x256", "512x512", or "1024x1024"',
    `generate_number`    int           NOT NULL DEFAULT '1' COMMENT 'The number of images to generate. Must be between 1 and 10. Defaults to 1.',
    `original_image`     varchar(32)   NOT NULL DEFAULT '' COMMENT 'The original image uuid,interacting_method must be 2/3',
    `mask_image`         varchar(32)   NOT NULL DEFAULT '' COMMENT 'The mask image uuid,interacting_method must be 2',
    `resp_images_path`   varchar(2048) NOT NULL DEFAULT '' COMMENT 'The url of the generated images which from openai response,separated by commas',
    `generated_images`   varchar(512)  NOT NULL DEFAULT '' COMMENT 'The uuid of the generated images,separated by commas',
    `interacting_method` smallint      NOT NULL DEFAULT '1' COMMENT '1:Creating images from scratch based on a text prompt;2:Creating edits of an existing image based on a new text prompt;3:Creating variations of an existing image',
    `process_status`  smallint      NOT NULL DEFAULT '1' COMMENT 'Process status,1:processing,2:fail,3:success',
    `create_time`        datetime      NOT NULL DEFAULT CURRENT_TIMESTAMP,
    `update_time`        datetime      NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    `is_delete`          tinyint       NOT NULL DEFAULT '0',
    PRIMARY KEY (`id`),
    UNIQUE KEY `udx_uuid` (`uuid`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4
  COLLATE = utf8mb4_general_ci COMMENT ='Images generated by ai';

CREATE TABLE `adi_user`
(
    `id`                              bigint       NOT NULL AUTO_INCREMENT,
    `name`                            varchar(45)  NOT NULL DEFAULT '',
    `password`                        varchar(120) NOT NULL DEFAULT '',
    `uuid`                            varchar(32)  NOT NULL DEFAULT '',
    `email`                           varchar(120) NOT NULL DEFAULT '',
    `active_time`                     datetime     NULL COMMENT '激活时间',
    `secret_key`                      varchar(120) NOT NULL default '' COMMENT 'Custom openai secret key',
    `understand_context_msg_pair_num` int          NOT NULL default '0' COMMENT '上下文理解中需要携带的消息对数量（提示词及回复）',
    `quota_by_token_daily`            int          NOT NULL DEFAULT '0' COMMENT 'The quota of token daily',
    `quota_by_token_monthly`          int          NOT NULL DEFAULT '0' COMMENT 'The quota of token monthly',
    `quota_by_request_daily`          int          NOT NULL DEFAULT '0' COMMENT 'The quota of http request daily',
    `quota_by_request_monthly`        int          NOT NULL DEFAULT '0' COMMENT 'The quota of http request monthly',
    `quota_by_image_daily`            int          NOT NULL DEFAULT '0' COMMENT 'The quota of generate images daily',
    `quota_by_image_monthly`          int          NOT NULL DEFAULT '0' COMMENT 'The quota of generate images monthly',
    `create_time`                     datetime     NOT NULL DEFAULT CURRENT_TIMESTAMP,
    `update_time`                     datetime     NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    `user_status`                     smallint     NOT NULL DEFAULT '1' COMMENT '用户状态，1：待验证；2：正常；3：冻结',
    `is_delete`                       tinyint      NOT NULL DEFAULT '0' COMMENT '0：未删除；1：已删除',
    PRIMARY KEY (`id`),
    UNIQUE KEY `udx_uuid` (`uuid`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4
  COLLATE = utf8mb4_general_ci COMMENT ='用户表';

CREATE TABLE `adi_user_day_cost`
(
    `id`              bigint   NOT NULL AUTO_INCREMENT,
    `user_id`         bigint   NOT NULL DEFAULT '0',
    `day`             int      NOT NULL DEFAULT '0' COMMENT '日期,用7位整数表示,如20230901',
    `requests`        int      NOT NULL DEFAULT '0' COMMENT 'The number of http request',
    `tokens`          int      NOT NULL DEFAULT '0' COMMENT 'The cost of the tokens',
    `images_number`   int      NOT NULL DEFAULT '0' COMMENT 'The number of images',
    `secret_key_type` int      NOT NULL DEFAULT '1' COMMENT '1:System secret key,2:Custom secret key',
    `create_time`     datetime NOT NULL DEFAULT CURRENT_TIMESTAMP,
    `update_time`     datetime NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    PRIMARY KEY (`id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4
  COLLATE = utf8mb4_general_ci COMMENT ='用户每天消耗总量表';


CREATE TABLE `adi_prompt`
(
    `id`          bigint        NOT NULL AUTO_INCREMENT,
    `user_id`     bigint        NOT NULL DEFAULT '0' COMMENT '0:System,other:User',
    `act`         varchar(120)  NOT NULL DEFAULT '' COMMENT 'Short prompt for search/autocomplete',
    `prompt`      varchar(1024) NOT NULL COMMENT 'Prompt content',
    `create_time` datetime      NOT NULL DEFAULT CURRENT_TIMESTAMP,
    `update_time` datetime      NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    `is_delete`   tinyint       NOT NULL DEFAULT '0' COMMENT '0:Normal;1:Deleted',
    PRIMARY KEY (`id`),
    KEY `idx_title` (`act`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4
  COLLATE = utf8mb4_general_ci COMMENT ='提示词';

CREATE TABLE `adi_file`
(
    `id`          bigint       NOT NULL AUTO_INCREMENT,
    `name`        varchar(32)  NOT NULL DEFAULT '',
    `uuid`        varchar(32)  NOT NULL DEFAULT '',
    `md5`         varchar(128)  NOT NULL DEFAULT '',
    `ext`         varchar(32)  NOT NULL DEFAULT '',
    `user_id`     bigint       NOT NULL DEFAULT '0' COMMENT '0:System,other:User',
    `path`        varchar(250) NOT NULL DEFAULT '',
    `ref_count`   int          NOT NULL DEFAULT '0' COMMENT 'The number of references to this file',
    `create_time` datetime     NOT NULL DEFAULT CURRENT_TIMESTAMP,
    `update_time` datetime     NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    `is_delete`   tinyint      NOT NULL DEFAULT '0' COMMENT '0:Normal;1:Deleted',
    PRIMARY KEY (`id`),
    KEY `idx_uuid` (`uuid`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4
  COLLATE = utf8mb4_general_ci COMMENT ='提示词';