# Contributing Guidelines

Thank you for your interest in LangChain4j-AIDeepin! We welcome all forms of contribution.

## How to Contribute

- **Bug Reports** — Open an Issue describing the problem and steps to reproduce
- **Feature Suggestions** — Open an Issue describing the use case and expected behavior
- **Documentation Improvements** — Submit a PR directly to fix errors or add details
- **Code Contributions** — Follow the process below to submit a PR

## Code Contributions

> **TL;DR** — You're good to submit a PR if you:
> - Pass the project's built-in lint checks (frontend) or Sonar / similar tool checks (backend)
> - Verify successful compilation and local execution
> - Keep your code style consistent with the existing codebase

### Branch Naming

- `feature/xxx` — New features
- `fix/xxx` — Bug fixes
- `docs/xxx` — Documentation updates
- `refactor/xxx` — Code refactoring

Create branches from `main`.

### Commit Convention

Follow the [Conventional Commits](https://www.conventionalcommits.org/) format:

```
<type>(<scope>): <subject>
```

**Types:**

| Type | Description |
|:-----|:------------|
| feat | New feature |
| fix | Bug fix |
| docs | Documentation update |
| style | Code formatting (no logic changes) |
| refactor | Refactoring (not a new feature or fix) |
| perf | Performance improvement |
| test | Test related |
| build | Build or dependency changes |
| ci | CI configuration changes |
| chore | Miscellaneous |
| revert | Revert a commit |
| wip | Work in progress |
| workflow | Workflow related |
| types | Type definition changes |
| release | Release version |

**Scope (optional):** `server`, `admin-web`, `user-web`, `docker`

**Subject:** Concise description of the change, max 108 characters.

**Examples:**

```
feat(server): add DeepSeek model adapter
fix(admin-web): fix knowledge base list pagination issue
docs: update deployment guide
```

### Code Style

Overall, keep your code style consistent with the existing codebase.

#### Backend (Java)

- Use your IDE's default formatting
- JDK 17 language features

#### Admin Web (admin-web)

- Prettier + ESLint: 2-space indent, no semicolons, single quotes
- Check: `pnpm lint`
- Auto-fix: `pnpm lint:fix`

#### User Web (user-web)

- @antfu ESLint: tab indentation
- Check: `pnpm lint`
- Auto-fix: `pnpm lint:fix`

### PR Process

1. Fork this repository
2. Create a feature branch from `main` (e.g. `feature/add-xxx`)
3. Commit your code following the commit convention above
4. Make sure the following checks pass before submitting your PR:
   - **Backend**: `mvn compile` succeeds, application starts without errors
   - **Frontend**: `pnpm lint` passes, `pnpm build` succeeds
5. Push to your fork
6. Open a Pull Request against the `main` branch

**PR description should include:**

- Summary of changes
- Related Issue number (e.g. `Closes #123`)
- Testing instructions (how to verify the changes)

## Issue Guidelines

### Bug Reports

Please include:

- **Environment**: OS, JDK version, browser, project version
- **Steps to Reproduce**: Step-by-step description of how to trigger the issue
- **Expected Behavior**: What you expected to happen
- **Actual Behavior**: What actually happened (screenshots or logs are helpful)

### Feature Requests

Please include:

- **Background**: The problem or use case you're facing
- **Expected Behavior**: How you'd like it to be resolved
- **Alternatives**: Other approaches you've considered (optional)
