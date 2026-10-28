# Upgrade

> [← Deployment](index.md) · [简体中文](../../cn/deployment/upgrade.md)

Upgrading is intended for deployers with development experience and is an atomic process you orchestrate: the "database migration SQL → backend → frontend" replacement must be completed within the same maintenance window. Any intermediate state mixing old and new artifacts can break things — for example, API changes in a new backend can break the old frontend, and once a migration that renames tables has been executed, queries from the old backend fail immediately.

## 1. Determine Current and Target Versions

- Your currently deployed version defines the migration starting point: every incremental script (`NNN_*.sql`) numbered no greater than the current version has already been applied
- The migrations to run are the incremental scripts added between the two versions:

  ```bash
  git diff --name-only <current>..<target> -- server/db_migration
  ```
- Migration scripts live in [server/db_migration](https://github.com/moyangzhan/langchain4j-aideepin/tree/main/server/db_migration); see that directory's [README](https://github.com/moyangzhan/langchain4j-aideepin/blob/main/server/db_migration/README.md) for naming conventions and how to run them

## 2. Prepare

1. Back up the database — for disaster recovery (rebuilding if the upgrade fails or data is corrupted):

   ```bash
   pg_dump -h <host> -U <user> -d aideepin -F c -f aideepin_backup_$(date +%Y%m%d).dump
   ```

2. For Docker deployments, also update `IMAGE_VERSION` in `.env`: images are tagged with this value, so incrementing it per release keeps previous images available

## 3. Perform the Upgrade

Complete the steps in order within a maintenance window; the service is unavailable during it:

1. **Stop the services.** All-in-one: run `docker compose down` in the root `docker/` directory; per-service Docker: in each sub-project's `docker/` directory; bare-metal: stop the backend process
2. **Run database migrations**: starting from the number that corresponds to your current version, execute all incremental migration scripts in numeric order (see the db_migration README)
3. **Check out the target version**: `git checkout <tag or commit>`
4. **Rebuild and start**:
   - All-in-one: `docker compose up -d --build` in the root `docker/` directory
   - Per-service Docker: `docker compose up -d --build` in each sub-project's `docker/` directory
   - Bare-metal: `mvn clean package -Dmaven.test.skip=true`, replace the JAR, and start it (see [Bare-metal deployment](bare-metal.md))
5. **Verify**: sign in to the user web and admin web, and check that core features (chat, knowledge base, workflow) work

## 4. About Rollback

**Database rollback is not supported**: migration scripts are forward-only — there are no rollback scripts, applied migrations cannot be undone, and the schema does not revert when the backend does. Any combination of "old backend/frontend + migrated database" is unsupported — even additive-looking migrations may modify data the old version depends on. Therefore:

- Rehearse the full upgrade in a test environment before touching production
- The pre-upgrade database backup is for disaster recovery (rebuilding the database, recovering data), not for version rollback; restoring it discards everything produced after the upgrade and is an at-your-own-risk manual operation

---

Previous: [Bare-metal deployment](bare-metal.md)
