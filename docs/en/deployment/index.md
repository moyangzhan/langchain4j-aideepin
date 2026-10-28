# Deployment

For developers responsible for deploying and upgrading AIDeepIn: the complete flow from environment setup and fresh installation to version upgrades.

## Fresh Deployment

1. [All-in-one Docker Compose](quick-start.md): single-host quick deployment — backend + Redis + both web apps + nginx with one command
2. [Per-service Docker deployment](docker-split.md): deploy each service with its own compose stack (e.g. admin web on an intranet, user web on a public network)
3. [Bare-metal deployment](bare-metal.md): no Docker — run the backend as a JAR and host the web build outputs yourself

## Upgrading

4. [Upgrade](upgrade.md): the atomic procedure — backup, stop, database migration, backend/frontend replacement; database rollback is not supported, rehearse in a test environment first

---

> [← Back to overview](../index.md) · [简体中文](../../cn/deployment/index.md)
