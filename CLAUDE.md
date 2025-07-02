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