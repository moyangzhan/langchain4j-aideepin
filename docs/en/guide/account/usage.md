# Quota & Usage

> [← User Guide](../index.md) · [简体中文](../../../cn/guide/account/usage.md)

Check your consumption under **Settings → Quota**. Data is grouped into **paid / free model** tabs, each with two cards.

## Text Chat

| Metric | Description |
|---|---|
| Today's requests | Chat / Q&A requests today |
| Today's tokens | Input + output tokens today |
| This month's requests | Requests this month |
| This month's tokens | Token consumption this month |

Metrics with a "/ limit" suffix are quota-bound (e.g. "128 / 1000"); hitting the limit blocks the capability for the rest of the day / month.

## Image Generation

| Metric | Description |
|---|---|
| Today's images | Images generated today |
| This month's images | This month's total |

## Notes

- Free models are usually separately limited; paid models consume platform tokens — choosing a free model for a task eases quota pressure;
- Quotas are set by the admin across **four dimensions** — tokens, requests, images, chat questions — each with daily and monthly caps (see [Admin Console](../admin/admin.md#quota-configuration)); "Unlimited token quota" means no cap;
- Besides quotas there is **rate limiting** (requests per time window) — triggered limits ask you to retry later;
- When exceeded, the affected features report a limit — contact the admin to adjust.

---

Previous: [Sign-up, Login & Settings](account.md) · Next: [Prompt Store](../prompt-store.md)
