-- Add locale column to adi_user table
ALTER TABLE adi_user ADD COLUMN IF NOT EXISTS locale character varying(10) DEFAULT '' NOT NULL;
COMMENT ON COLUMN adi_user.locale IS '用户语言偏好，空字符串表示跟随全局设置 | User locale preference, empty string means follow global setting';
