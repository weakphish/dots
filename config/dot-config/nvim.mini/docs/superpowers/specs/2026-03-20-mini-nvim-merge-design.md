# Mini.nvim Config Merge Design

## Overview

Merge select plugins from the original nvim config into the mini.nvim config (nvim.mini). External plugins are already handled in `plugin/40_plugins.lua` — extend that file with new plugins and fill in existing stubs. New keymaps go in `plugin/20_keymaps.lua`.

## Approach

The mini config already separates concerns:
- `plugin/10_options.lua` — Neovim options
- `plugin/20_keymaps.lua` — all keymaps
- `plugin/30_mini.lua` — mini modules
- `plugin/40_plugins.lua` — external (non-mini) plugins

All changes go into the existing files. No new directories or files needed.

## Files Modified

```
plugin/40_plugins.lua    # Fill in conform formatters, uncomment LSP/Mason/Gruvbox, add new plugins
plugin/20_keymaps.lua    # Add keymaps for new plugins + clue groups
plugin/30_mini.lua       # Remove miniwinter colorscheme (replaced by gruvbox)
```

## Plugin Specifications

### 1. Gruvbox (colorscheme)

- **Plugin**: ellisonleao/gruvbox.nvim
- **Location**: `plugin/40_plugins.lua` — uncomment the existing gruvbox stub (lines 161-169)
- **Config**: Dark mode, `vim.cmd('colorscheme gruvbox')`
- **Also**: Remove/disable the miniwinter colorscheme in `plugin/30_mini.lua`
- **Loading**: `MiniDeps.now()` (must load immediately)

### 2. Conform.nvim (formatter)

- **Plugin**: stevearc/conform.nvim (already added in 40_plugins.lua)
- **Changes**: Fill in the existing `formatters_by_ft` table (line 128)
- **Formatters**:
  - `lua`: `{ 'stylua' }`
  - `python`: `{ 'isort', 'black' }` (sequential)
  - `rust`: `{ 'rustfmt' }`
  - `javascript`, `typescript`: `{ 'prettierd', 'prettier', stop_after_first = true }`
  - `go`: `{ 'gofmt' }`
- **Add**: `format_on_save = { timeout_ms = 500 }`
- **Exclude YAML from format_on_save**: Original config deliberately turned this off (commit 8bb2f56). Add `yaml = {}` to formatters_by_ft or use `format_on_save` as a function that skips YAML.
- **Keymaps**: `<Leader>lf` already mapped in 20_keymaps.lua — no changes needed

### 3. LazyGit

- **Plugin**: kdheepak/lazygit.nvim
- **Dependencies**: None (plenary no longer required in recent versions)
- **Location**: New section in `plugin/40_plugins.lua`
- **Loading**: `later()` (on-demand)
- **Keymaps** (in 20_keymaps.lua):
  - `<Leader>gg` — open LazyGit

### 4. LSP Servers + Mason

- **Plugins**: mason-org/mason.nvim (already stubbed in 40_plugins.lua), neovim/nvim-lspconfig (already added)
- **Changes**:
  - Uncomment Mason stub (lines 153-156)
  - Uncomment `vim.lsp.enable()` (lines 101-103) and populate with servers
- **Approach**: `vim.lsp.enable()` (Neovim 0.11+ native), NOT mason-lspconfig
- **LSP Servers** (exact names from original config):
  - `ty` (Typst)
  - `ruff` (Python linting)
  - `ts_ls` (TypeScript/JavaScript)
  - `gopls` (Go)
  - `bashls` (Bash, with custom filetypes config: `{"bash", "sh", "zsh"}`)
  - `rust_analyzer` (Rust)
  - `actionlint` (GitHub Actions linting)
  - `gh-actions-language-server` (GitHub Actions)
  - `yaml-language-server` (YAML)
  - `lua_ls` (already configured in `after/lsp/lua_ls.lua`)
- **Note on server names**: The original config uses `gh-actions-language-server` and `yaml-language-server` directly with `vim.lsp.enable()`. These may resolve via lsp config files in `after/lsp/` or Neovim 0.11+ mechanisms. If they don't work, the lspconfig canonical names are `github_actions_ls` and `yamlls`. Verify after implementation.
- **Note on actionlint**: This is a linter, not a traditional LSP server. The original config enables it via `vim.lsp.enable()`. If it doesn't work, it may need nvim-lint instead. Verify after implementation.
- **Note on bashls**: Original config had `".zshrc"` as a filetype, which is a filename not a filetype. Corrected to `"zsh"`.
- **Loading**: `now_if_args()` (same as existing LSP block)
- **Keymaps**: Uses existing `<Leader>l` group — no changes needed

### 5. Treesitter Context

- **Plugin**: nvim-treesitter/nvim-treesitter-context
- **Location**: New section in `plugin/40_plugins.lua`, inside the existing treesitter `now_if_args` block
- **Loading**: `now_if_args()` (loads with file arguments, same as treesitter)
- **Config**: Default settings (sticky function/class header at window top)
- **Keymaps**: None needed

### 6. Trouble.nvim

- **Plugin**: folke/trouble.nvim
- **Location**: New section in `plugin/40_plugins.lua`
- **Loading**: `later()` (on-demand)
- **Keymaps** (in 20_keymaps.lua, new `<Leader>x` group):
  - `<Leader>xx` — toggle workspace diagnostics (`Trouble diagnostics toggle`)
  - `<Leader>xX` — toggle buffer diagnostics (`Trouble diagnostics toggle filter.buf=0`)
  - `<Leader>xq` — quickfix list (`Trouble qflist toggle`)
  - `<Leader>xl` — location list (`Trouble loclist toggle`)
  - `<Leader>xt` — TODOs (`Trouble todo toggle`)
  - `<Leader>xs` — document symbols (`Trouble symbols toggle focus=false`)
- **Integration**: mini.hipatterns stays for inline TODO/FIXME highlighting; Trouble provides the aggregated panel

### 7. Grug-far

- **Plugin**: MagicDuck/grug-far.nvim
- **Location**: New section in `plugin/40_plugins.lua`
- **Loading**: `later()` (on-demand)
- **Keymaps** (in 20_keymaps.lua, new `<Leader>r` group):
  - `<Leader>rr` — search and replace (cursor word)

## Integration: mini.clue

Add these entries to `Config.leader_group_clues` in `plugin/20_keymaps.lua`:

```lua
{ mode = 'n', keys = '<Leader>r', desc = '+Replace' },
{ mode = 'n', keys = '<Leader>x', desc = '+Trouble' },
```

## Integration: Colorscheme swap

1. In `plugin/30_mini.lua`: Remove or comment out the `MiniHues` / `miniwinter` / `randomhue` colorscheme setup
2. In `plugin/40_plugins.lua`: Uncomment the gruvbox block, enable only gruvbox, set `vim.cmd('colorscheme gruvbox')`

## Integration: Treesitter languages

The existing `languages` table in 40_plugins.lua only has `lua`, `vimdoc`, `markdown`. Expand it to match the LSP servers being added:

```lua
local languages = {
  'lua', 'vimdoc', 'markdown',
  'python', 'typescript', 'javascript', 'go', 'rust', 'bash', 'yaml',
  'tsx', 'html', 'css', 'json', 'toml', 'diff', 'typst',
}
```

## What stays unchanged

- All existing mini modules and their configs (except colorscheme removal)
- Treesitter setup (just adding context plugin alongside it)
- mini.hipatterns (inline TODO highlighting stays, Trouble adds panel view)
- mini.diff + mini.git (stay as-is, LazyGit is supplementary)
- All existing keymaps not explicitly overridden
- `after/lsp/lua_ls.lua`
- Snippet setup (friendly-snippets + mini.snippets)
