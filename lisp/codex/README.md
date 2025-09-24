# Codex IDE

Codex IDE integrates the Codex CLI with Emacs to provide project-scoped AI coding
sessions that mirror the workflow of `claude-code-ide`.

## Files
- `codex-ide.el` — session lifecycle management built on top of `vterm` and `project`.
- `codex-transient.el` — transient menu for starting, resuming, and configuring sessions.

## Features
- Launch, resume, and stop Codex CLI sessions per project using persistent buffers.
- Optional sandbox flag, default model selection, and extra CLI arguments.
- Quick access menu on `codex-ide-menu` (bound to `C-c C-"` via `init.el`).
- Debug buffer logging and helpers for verifying Codex CLI availability.

## Usage
1. Call `codex-ide-start` within a project to spawn a new Codex terminal buffer.
2. Use `codex-ide-resume` or `codex-ide-resume-with-selection` to reconnect to existing conversations.
3. Open the transient menu (`codex-ide-menu`) to toggle settings, inspect sessions, or jump to buffers.

## Customisation
The following user options control runtime behaviour:
- `codex-ide-command` — path to the Codex CLI binary.
- `codex-ide-default-model` — model name passed to new sessions (nil delegates to CLI default).
- `codex-ide-sandbox-mode` — toggle sandbox flag when launching sessions.
- `codex-ide-extra-args` — additional CLI arguments appended to every invocation.
- `codex-ide-auto-switch-to-buffer` — automatically focus the session buffer after launching.

Adjust these variables with `M-x customize-group RET codex-ide RET` or through the
settings submenu in the transient menu.

## Development Notes
- `codex-ide` stores active sessions in a hash table keyed by project root.
- Buffers are created with `vterm` and keep logs even after the underlying process exits.
- Tests and additional helper modules should live under `lisp/tests/` mirroring this directory.
