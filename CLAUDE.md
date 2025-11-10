# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package Management

This Emacs configuration uses **straight.el** as the primary package manager instead of the default package.el. All packages are installed via `use-package` with `straight-use-package-by-default` set to `t`.

Key package management commands:
- `M-x straight-use-package` - Install a package directly
- `M-x straight-rebuild-package` - Rebuild a specific package
- `M-x straight-pull-all` - Update all packages
- `M-x straight-freeze-versions` - Lock package versions

## Architecture Overview

### Main Configuration Files
- `init.el` - Primary configuration file with all package configurations and settings
- `early-init.el` - Contains early initialization settings (LSP_USE_PLISTS environment variable)
- `lisp/` - Custom local packages and extensions:
  - `auth-source-ghcli.el` - GitHub CLI authentication integration
  - `check-asdf-path.el` - asdf environment diagnostics and auto-fix tools
  - `sql-ts-mode.el` - SQL tree-sitter mode
  - `vitest.el` - Vitest integration
  - `gotest-dape.el` - Go testing with DAP integration

### Key Package Categories

**Language Support:**
- LSP via `lsp-mode` with language-specific configurations
- Tree-sitter modes for modern syntax highlighting
- Language-specific packages for Ruby, Go, JavaScript/TypeScript, Python, Dart/Flutter

**Completion & Navigation:**
- Vertico + Orderless for fuzzy completion
- Corfu for in-buffer completion
- Consult for enhanced search/navigation
- Projectile for project management

**Development Tools:**
- Magit for Git integration
- Flycheck for syntax checking
- Copilot for AI assistance
- Claude Code integration for AI-powered development

**Terminal Integration:**
- Multiple terminal backends: eat, vterm, eshell
- Terminal backend configurable via `claude-code-terminal-backend` variable

## Development Workflow

### Environment Management with asdf

This configuration relies on **asdf** for managing runtime versions (Node.js, Ruby, Go, Python, etc.). Proper asdf integration is critical for tools like Copilot, LSP servers, and language-specific utilities to function correctly.

#### Key Requirements

1. **exec-path-from-shell** must initialize on Linux/macOS
   - Configured in `init.el` to load asdf environment variables
   - Loads: `PATH`, `MANPATH`, `ASDF_DIR`, `ASDF_DATA_DIR`
   - Should execute on all non-Windows systems

2. **Project-specific `.tool-versions`**
   - Located at `~/.emacs.d/.tool-versions`
   - Ensures consistent tool versions when processes start from `~/.emacs.d`
   - Example:
     ```
     nodejs 22.21.1
     ruby 3.1.6
     ```

3. **Global npm packages must be installed per Node.js version**
   - When switching Node.js versions with asdf, reinstall global packages
   - Example: `npm install -g @github/copilot-language-server`
   - asdf reshims automatically after npm global installs

#### Common Issues

**Symptom**: Tool not found or "No version is set" error
- **Cause**: Tool installed in old Node.js/Ruby version
- **Solution**: Install in current version or run `asdf reshim <plugin>`

**Symptom**: Copilot server crashes with exit code 126
- **Cause**: asdf environment not loaded, or copilot-language-server not in current Node.js version
- **Solution**:
  1. Check environment: `M-x check-asdf-environment`
  2. Reinstall if needed: `npm install -g @github/copilot-language-server`
  3. Restart Emacs

**Symptom**: Different behavior in terminal vs Emacs
- **Cause**: Emacs not loading shell environment
- **Solution**: Verify `exec-path-from-shell-initialize` runs on startup

#### Diagnostic Tools

**Check asdf environment:**
```elisp
M-: (load-file "~/.emacs.d/lisp/check-asdf-path.el") RET
M-x check-asdf-environment RET
```

The diagnostic buffer shows:
- Environment variables (ASDF_DIR, ASDF_DATA_DIR, PATH)
- Executable paths and whether they're asdf shims
- Node.js execution tests
- Summary of any issues

**Manual checks:**
```elisp
M-: (getenv "ASDF_DIR") RET           # Should return ~/.asdf or similar
M-: (getenv "PATH") RET               # Should contain asdf paths
M-: (executable-find "node") RET      # Should return asdf-managed node
```

### Authentication Setup
The configuration integrates with various authentication sources:
- macOS Keychain (when on macOS)
- KWallet (when available)
- GitHub CLI authentication
- API keys stored in auth-source for OpenAI, OpenRouter, and GitHub

### AI Integration
Multiple AI services are configured:
- **Copilot**: Integrated with `copilot-mode` for inline suggestions
- **Claude Code**: Configured with Anthropic API key from auth-source
- **ChatGPT Shell**: Available for interactive AI conversations  
- **Emigo**: Alternative AI backend using OpenRouter
- **MCP Hub**: Model Context Protocol integration with GitHub and fetch servers

### Language-Specific Notes

**Ruby**: Full Rails development setup with projectile-rails, rspec-mode, and rubocop integration
**Go**: Includes go-mode with project detection, testing tools, and golangci-lint
**JavaScript/TypeScript**: Jest integration, coverage reporting, and Biome LSP for formatting
**SQL**: Custom tree-sitter mode with LSP support

## Common Commands

### Package Management
```
M-x straight-use-package RET package-name  # Install package
M-x straight-rebuild-all                   # Rebuild all packages
M-x straight-pull-all                      # Update all packages
```

### Project Navigation
```
C-x p     # Projectile command map
C-c g     # Consult git grep
C-x C-r   # Recent files
C-x C-b   # Buffer list
```

### Development
```
C-c c     # Claude Code command map
C-c l     # LSP command map
C-c s     # RSpec command map (Ruby)
C-c d     # Docker commands
```

### AI Assistance
```
C-c c     # Claude Code integration
C-M-;     # Copilot complete
TAB       # Accept Copilot suggestion (custom binding)
```

## Testing

Language-specific testing is integrated:
- **Ruby**: RSpec mode with `C-c s` prefix
- **Go**: gotest integration
- **JavaScript**: Jest mode with coverage reporting
- **General**: Compilation mode with enhanced error parsing

No unified test runner - use language-specific commands or compile mode for custom test scripts.

## Troubleshooting

### asdf Environment Issues

When experiencing issues with tools not being found or version mismatches, follow this workflow:

#### Quick Diagnosis

```elisp
M-x asdf-quick-fix RET
```

This command checks for common issues and suggests fixes automatically.

#### Detailed Environment Check

```elisp
M-x check-asdf-environment RET
```

Opens a detailed report showing:
- All environment variables
- Executable paths and their types (shim vs direct)
- Node.js execution tests
- copilot-language-server details

#### Common Fix Commands

**Reinstall copilot-language-server:**
```elisp
M-x asdf-fix-copilot-server RET
```
Reinstalls copilot-language-server for the current Node.js version.

**Reshim all asdf plugins:**
```elisp
M-x asdf-reshim-all RET
```
Rebuilds all asdf shims after installing global packages.

**Verify .tool-versions:**
```elisp
M-x asdf-verify-tool-versions RET
```
Checks if versions listed in `.tool-versions` are actually installed.

#### Troubleshooting Workflow

1. **Symptom**: Tool crashes or "not found" errors
   ```elisp
   M-x asdf-quick-fix RET
   ```
   Follow the suggested fixes.

2. **Symptom**: Different behavior in terminal vs Emacs
   ```elisp
   M-: (getenv "PATH") RET
   ```
   Verify PATH contains asdf directories. If not, restart Emacs.

3. **Symptom**: Copilot server fails with exit code 126 or 1
   ```elisp
   M-x check-asdf-environment RET  ; Check environment
   M-x asdf-fix-copilot-server RET ; Fix if needed
   ```
   Then restart Emacs.

4. **Symptom**: After upgrading Node.js version
   ```bash
   # In terminal
   npm install -g @github/copilot-language-server
   asdf reshim nodejs
   ```
   Then restart Emacs.

#### Prevention

- Always maintain `~/.emacs.d/.tool-versions` with current versions
- After changing asdf versions, reinstall global npm packages
- Restart Emacs after significant environment changes
- Run `M-x check-asdf-environment` after updating asdf or installing new tools

### Copilot-Specific Issues

**Check Copilot logs:**
- `*copilot stderr*` buffer - Server errors and warnings
- `*copilot events*` buffer - JSONRPC communication log (when `copilot-log-max` > 0)

**Authentication:**
```elisp
M-x copilot-diagnose RET  ; Check authorization status
M-x copilot-login RET     ; Re-authenticate if needed
```

**Server status:**
```elisp
M-x copilot-reinstall-server RET  ; Reinstall if corrupted
```