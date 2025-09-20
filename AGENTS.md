# Repository Guidelines

## Project Structure & Module Organization
Core configuration lives in `init.el`, with early environment setup in `early-init.el`. Custom modules reside in `lisp/`, each file exposing a focused feature (e.g., `auth-source-ghcli.el` for GitHub auth, `vitest.el` for JS testing helpers). Package builds synced by straight.el populate `straight/`; do not edit its contents manually. Runtime caches and user data sit in `var/`, `auto-save-list/`, and tool-specific directories such as `chatgpt/` and `copilot-chat/`. Keep new agent integrations in dedicated folders and load them from `init.el` via `use-package` to preserve the current modular layout.

## Build, Test, and Development Commands
- `emacs --debug-init` — launch with this config and capture initialization stack traces.
- `emacs -Q --load early-init.el --load init.el` — reproduce a clean startup without inherited user state.
- `M-x straight-pull-all` — upgrade all tracked packages; follow with a smoke test.
- `M-x straight-freeze-versions` — lock package revisions before shipping coordinated changes.
- `M-x auto-package-update-now` — refresh packages on demand when verifying fixes.

## Coding Style & Naming Conventions
Write new Emacs Lisp with `lexical-binding: t`, rely on `use-package` blocks for feature setup, and keep side-effectful code inside `:config`. Follow Emacs’ default Lisp indentation (two spaces) and keep lines under 100 characters. Name modules and defuns with hyphenated prefixes that reflect their namespace (e.g., `vitest-runner-*`). Store secrets in auth-source backends, never in repo files. Add terse `;;;` section headers when grouping related settings.

## Testing Guidelines
Prefer `ert` for regression coverage; co-locate test helpers under `lisp/tests/` mirroring the feature namespace. Name tests `module-test/behavior` for quick filtering. Run suites non-interactively with `emacs --batch -l ert -l lisp/tests/module-tests.el -f ert-run-tests-batch-and-exit`. For language-specific tooling (Go, Ruby, JS), verify their external runners from within Emacs using the provided minor modes before opening a PR.

## Commit & Pull Request Guidelines
Follow Conventional Commits with scoped prefixes (`feat(init): …`, `refactor(lisp): …`). Summaries should stay in English and describe user-visible impact; add context lines for rationale or follow-ups. Each PR should include: purpose, manual test notes (e.g., startup command used), screenshots for UI tweaks, and linked issues if applicable. Avoid committing generated caches under `var/` or `straight/`; run `git status` before pushing.

## Security & Configuration Tips
Never commit API keys or tokens; rely on `authinfo.gpg`, macOS Keychain, or KWallet bindings already configured in `init.el`. When introducing new services, document required environment variables and prefer per-machine setup instructions over code changes. Review `exec-path-from-shell` usage if new binaries are needed so shells and Emacs stay aligned.
