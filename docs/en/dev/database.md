# Database Design Conventions

Applies to new tables and new reference columns; existing tables are not refactored.

## Base columns

Every new table carries two keys plus three audit columns:

| Column | Type | Role |
|---|---|---|
| `id` | `bigserial` primary key | In-database usage: joins, ownership checks, cascade cleanup |
| `uuid` | `varchar(32)`, no hyphens | Public identity: URLs, APIs, cross-store addressing (vector store / graph store / Redis / queues) |
| `create_time` | `timestamp`, default `CURRENT_TIMESTAMP` | Creation time |
| `update_time` | `timestamp`, default `CURRENT_TIMESTAMP` | Last update time, overwritten by a BEFORE UPDATE trigger (`trigger_<table>_update_time`) |
| `is_deleted` | `boolean`, default `false` | Soft-delete flag |

Notes:

- `create_time` / `update_time` / `is_deleted` are all owned by the database (DDL defaults); the Java side (BaseEntity) never writes them, it only re-reads;
- `is_deleted` may be omitted, e.g. in the `adi_document_graph_vertex` table: its rows can be re-extracted from tables such as `adi_document_segment`, so hard delete is acceptable.
- The `uuid` version is **uuid v7** (existing v4 values coexist with v7, no migration).

## Foreign keys

- Format: `xx_id`
- Add the redundant foreign-key column `xx_uuid` (`varchar(32)`) as actual needs require

## External bindings: uuid only

**URLs (front-end routes, API paths), file names, and object-storage keys — anything bound to table data — must use uuid, never id.**

- An id in a URL is sequential and enumerable; business volume can be scraped by traversal;
- A file name is a reference persisted in storage;
- Values exported to the vector store, Redis, or queues follow the same rule.

## Review checklist

- An id appearing in a URL / file name / storage key → violation, raise it.

---

Previous: [ASR Integration](capability-asr.md) · Next: [Back to contents](../index.md)
