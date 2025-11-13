# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package Management

This Emacs configuration uses **straight.el** as the primary package manager instead of the default package.el. All packages are installed via `use-package` with `straight-use-package-by-default` set to `t`.

Key package management commands:
- `M-x straight-use-package` - Install a package directly
- `M-x straight-rebuild-package` - Rebuild a specific package
- `M-x straight-pull-all` - Update all packages
- `M-x straight-freeze-versions` - Lock package versions

## Development Philosophy

**Backward Compatibility:**
- This is a personal configuration repository
- Backward compatibility is NOT a concern
- Feel free to rename functions, change interfaces, or refactor code without maintaining old APIs
- Breaking changes are acceptable and encouraged if they improve the codebase
- No need to provide compatibility aliases or deprecation warnings

## Architecture Overview

### Main Configuration Files
- `init.el` - Primary configuration file with all package configurations and settings
- `early-init.el` - Contains early initialization settings (LSP_USE_PLISTS environment variable)
- `lisp/` - Custom local packages and extensions:
  - `auth-source-ghcli.el` - GitHub CLI authentication integration
  - `check-version-manager*.el` - Version manager (asdf/mise) environment diagnostics and auto-fix tools
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

### Environment Management with Version Managers (asdf/mise)

This configuration supports **asdf** and **mise** for managing runtime versions (Node.js, Ruby, Go, Python, etc.). Proper version manager integration is critical for tools like Copilot, LSP servers, and language-specific utilities to function correctly.

**Supported version managers:**
- **asdf** (https://asdf-vm.com) - Traditional shim-based approach
- **mise** (https://mise.jdx.dev) - Modern, faster alternative with direct PATH manipulation (recommended)

#### Key Requirements

1. **exec-path-from-shell** must initialize on Linux/macOS
   - Configured in `init.el` to load environment variables from your shell
   - Loads: `PATH`, `MANPATH`, and version manager-specific variables
   - Should execute on all non-Windows systems

2. **Project-specific version configuration**
   - Located at `~/.emacs.d/.tool-versions` (both asdf and mise) or `~/.emacs.d/.mise.toml` (mise only)
   - Ensures consistent tool versions when processes start from `~/.emacs.d`
   - Example `.tool-versions`:
     ```
     nodejs 22.21.1
     ruby 3.1.6
     ```

3. **Global npm packages must be installed per Node.js version**
   - When switching Node.js versions, reinstall global packages
   - Example: `npm install -g @github/copilot-language-server`
   - Version managers reshim automatically after npm global installs (asdf) or update PATH (mise)

#### Common Issues

**Symptom**: Tool not found or "No version is set" error
- **Cause**: Tool installed in old Node.js/Ruby version
- **Solution**:
  - asdf: Run `asdf reshim <plugin>`
  - mise: Paths update automatically; verify with `mise doctor`

**Symptom**: Copilot server crashes with exit code 126
- **Cause**: Version manager environment not loaded, or copilot-language-server not in current Node.js version
- **Solution**:
  1. Check environment: `M-x check-version-manager-environment`
  2. Reinstall if needed: `npm install -g @github/copilot-language-server`
  3. Restart Emacs

**Symptom**: Different behavior in terminal vs Emacs
- **Cause**: Emacs not loading shell environment
- **Solution**: Verify `exec-path-from-shell-initialize` runs on startup

#### Diagnostic Tools

**Check version manager environment:**
```elisp
M-x check-version-manager-environment RET
```

The diagnostic buffer shows:
- Detected version manager (asdf or mise)
- Environment variables
- Executable paths and whether they're managed by the version manager
- Node.js execution tests
- copilot-language-server location (PATH or Emacs cache)
- Summary of any issues

**Manual checks:**
```elisp
M-: (getenv "PATH") RET               # Should contain version manager paths
M-: (executable-find "node") RET      # Should return version manager-managed node
M-: (executable-find "mise") RET      # Check if mise is available
M-: (executable-find "asdf") RET      # Check if asdf is available
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

### Version Manager Environment Issues

When experiencing issues with tools not being found or version mismatches, follow this workflow:

#### Quick Diagnosis

```elisp
M-x version-manager-quick-fix RET
```

This command checks for common issues and suggests fixes automatically.

#### Detailed Environment Check

```elisp
M-x check-version-manager-environment RET
```

Opens a detailed report showing:
- Detected version manager (asdf or mise)
- All environment variables
- Executable paths and their management status
  - asdf: Checks if paths are shims (preferred)
  - mise: Checks if paths are direct (preferred)
- Node.js execution tests
- copilot-language-server details (PATH or Emacs cache)

#### Common Fix Commands

**Reinstall copilot-language-server:**
```elisp
M-x version-manager-fix-copilot-server RET
```
Reinstalls copilot-language-server for the current Node.js version. Works with both asdf and mise.

**Reshim all plugins (asdf/mise):**
```elisp
M-x version-manager-reshim-all RET
```
Rebuilds shims after installing global packages. Works with both asdf and mise.

**Verify version configuration:**
```elisp
M-x version-manager-verify-tool-versions RET
```
Checks if versions listed in `.tool-versions` or `.mise.toml` are actually installed.

#### Troubleshooting Workflow

1. **Symptom**: Tool crashes or "not found" errors
   ```elisp
   M-x version-manager-quick-fix RET
   ```
   Follow the suggested fixes.

2. **Symptom**: Different behavior in terminal vs Emacs
   ```elisp
   M-: (getenv "PATH") RET
   ```
   Verify PATH contains version manager directories. If not, restart Emacs.

3. **Symptom**: Copilot server fails with exit code 126 or 1
   ```elisp
   M-x check-version-manager-environment RET  ; Check environment
   M-x version-manager-fix-copilot-server RET ; Fix if needed
   ```
   Then restart Emacs.

4. **Symptom**: After upgrading Node.js version
   ```bash
   # In terminal
   npm install -g @github/copilot-language-server

   # If using asdf
   asdf reshim nodejs

   # If using mise (automatic, but can verify)
   mise doctor
   ```
   Then restart Emacs.

#### Prevention

- Always maintain `~/.emacs.d/.tool-versions` (or `.mise.toml` for mise) with current versions
- After changing versions, reinstall global npm packages
- Restart Emacs after significant environment changes
- Run `M-x check-version-manager-environment` after updating your version manager or installing new tools

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