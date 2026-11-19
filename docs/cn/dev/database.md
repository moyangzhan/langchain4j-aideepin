# 数据库设计规范

适用于新增表与新增引用列；存量表不改。

## 基础字段

每张新表固定携带两个键与三个审计列：

| 列 | 类型 | 职责 |
|---|---|---|
| `id` | `bigserial` 主键 | 库内使用：join、归属校验、级联清理 |
| `uuid` | `varchar(32)` 无连字符 | 公开身份：URL、API、跨存储（向量库 / 图库 / Redis / 队列）寻址 |
| `create_time` | `timestamp` `default CURRENT_TIMESTAMP` | 创建时间 |
| `update_time` | `timestamp` `default CURRENT_TIMESTAMP` | 最后修改时间，由 BEFORE UPDATE 触发器（`trigger_<表名>_update_time`）覆写 |
| `is_deleted` | `boolean` `default false` | 软删除标记 |

说明：
- `create_time` / `update_time` / `is_deleted` 均由数据库维护（DDL 提供 default），Java 端（BaseEntity）不写入、只回查读取；
- `is_deleted` 字段可省略，如 `adi_document_graph_vertex` 表，这是因为该表的数据可以由 `adi_document_segment` 等表的数据重新抽取生成，所以可以采取硬删除操作。
- `uuid` 版本为 **uuid v7**（存量 v4 值与 v7 并存，不做迁移）。

## 外键

- 格式： `xx_id`
- 根据实际情况增加冗余外键字段 `xx_uuid（varchar(32)）`

## 对外绑定：只使用 uuid

**URL（前端路由、API 路径）、文件名、对象存储 key，凡与表数据绑定的，只能使用 uuid，禁止使用 id。**

- URL 中的 id 连续可枚举，可被遍历推断业务量；
- 文件名是持久化在存储中的引用；
- 导出到向量库、Redis、队列的值遵循同一规则。

## Review 检查清单

- URL / 文件名 / 存储 key 中出现 id → 违规，提出。

---

上一篇：[语音识别（ASR）能力接入](capability-asr.md) ｜ 下一篇：[返回总目录](../index.md)
