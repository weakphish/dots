# Mini.nvim Config Merge Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Merge 7 external plugins (gruvbox, conform formatters, lazygit, LSP+mason, treesitter-context, trouble, grug-far) into the existing mini.nvim config.

**Architecture:** All changes go into existing `plugin/` files following the established pattern: options in `10_options.lua`, keymaps in `20_keymaps.lua`, mini modules in `30_mini.lua`, external plugins in `40_plugins.lua`. No new files or directories.

**Note:** All line numbers reference the original unmodified `plugin/40_plugins.lua`. Multiple tasks modify this file — apply changes using the "before" code blocks for matching rather than relying on line numbers after earlier tasks have run.

**Tech Stack:** Neovim, MiniDeps (plugin manager), mini.nvim ecosystem, vim.lsp.enable() (Neovim 0.11+)

**Spec:** `docs/superpowers/specs/2026-03-20-mini-nvim-merge-design.md`

---

### Task 1: Switch colorscheme from miniwinter to gruvbox

**Files:**
- Modify: `plugin/30_mini.lua:37` (comment out miniwinter)
- Modify: `plugin/40_plugins.lua:158-169` (uncomment gruvbox block)

- [ ] **Step 1: Comment out miniwinter in 30_mini.lua**

Replace line 37:
```lua
now(function() vim.cmd('colorscheme miniwinter') end)
```
With:
```lua
-- now(function() vim.cmd('colorscheme miniwinter') end)
```

- [ ] **Step 2: Uncomment and configure gruvbox in 40_plugins.lua**

Replace lines 158-169:
```lua
-- Beautiful, usable, well maintained color schemes outside of 'mini.nvim' and
-- have full support of its highlight groups. Use if you don't like 'miniwinter'
-- enabled in 'plugin/30_mini.lua' or other suggested 'mini.hues' based ones.
-- MiniDeps.now(function()
--   -- Install only those that you need
--   add('sainnhe/everforest')
--   add('Shatur/neovim-ayu')
--   add('ellisonleao/gruvbox.nvim')
--
--   -- Enable only one
--   vim.cmd('color everforest')
-- end)
```
With:
```lua
-- Color scheme =================================================================
MiniDeps.now(function()
  add('ellisonleao/gruvbox.nvim')
  require('gruvbox').setup({})
  vim.cmd('colorscheme gruvbox')
end)
```

- [ ] **Step 3: Verify**

Open nvim with a file: `nvim plugin/40_plugins.lua`
Expected: Gruvbox dark theme loads, no errors in `:messages`

- [ ] **Step 4: Commit**

```bash
git add plugin/30_mini.lua plugin/40_plugins.lua
git commit -m "Switch colorscheme from miniwinter to gruvbox"
```

---

### Task 2: Configure conform.nvim formatters

**Files:**
- Modify: `plugin/40_plugins.lua:114-130` (fill in conform config)

- [ ] **Step 1: Replace the conform setup block**

Replace lines 114-130:
```lua
later(function()
  add('stevearc/conform.nvim')

  -- See also:
  -- - `:h Conform`
  -- - `:h conform-options`
  -- - `:h conform-formatters`
  require('conform').setup({
    default_format_opts = {
      -- Allow formatting from LSP server if no dedicated formatter is available
      lsp_format = 'fallback',
    },
    -- Map of filetype to formatters
    -- Make sure that necessary CLI tool is available
    -- formatters_by_ft = { lua = { 'stylua' } },
  })
end)
```
With:
```lua
later(function()
  add('stevearc/conform.nvim')

  require('conform').setup({
    default_format_opts = {
      lsp_format = 'fallback',
    },
    formatters_by_ft = {
      lua = { 'stylua' },
      python = { 'isort', 'black' },
      rust = { 'rustfmt' },
      javascript = { 'prettierd', 'prettier', stop_after_first = true },
      typescript = { 'prettierd', 'prettier', stop_after_first = true },
      go = { 'gofmt' },
    },
    format_on_save = function(bufnr)
      -- Disable format on save for YAML
      if vim.bo[bufnr].filetype == 'yaml' then return end
      return { timeout_ms = 500 }
    end,
  })
end)
```

- [ ] **Step 2: Verify**

Open a Lua file: `nvim plugin/40_plugins.lua`
Run `:ConformInfo` — should show stylua configured for lua filetype.
Keymaps: `<Leader>lf` already wired in 20_keymaps.lua.

- [ ] **Step 3: Commit**

```bash
git add plugin/40_plugins.lua
git commit -m "Configure conform formatters with format-on-save"
```

---

### Task 3: Enable Mason and LSP servers

**Files:**
- Modify: `plugin/40_plugins.lua:94-104` (uncomment LSP enable)
- Modify: `plugin/40_plugins.lua:143-156` (uncomment Mason)

- [ ] **Step 1: Uncomment and configure Mason**

Replace lines 143-156:
```lua
-- Honorable mentions =========================================================

-- 'mason-org/mason.nvim' (a.k.a. "Mason") is a great tool (package manager) for
-- installing external language servers, formatters, and linters. It provides
-- a unified interface for installing, updating, and deleting such programs.
--
-- The caveat is that these programs will be set up to be mostly used inside Neovim.
-- If you need them to work elsewhere, consider using other package managers.
--
-- You can use it like so:
-- now_if_args(function()
--   add('mason-org/mason.nvim')
--   require('mason').setup()
-- end)
```
With:
```lua
-- Mason ========================================================================
now_if_args(function()
  add('mason-org/mason.nvim')
  require('mason').setup()
end)
```

- [ ] **Step 2: Uncomment and populate LSP enable**

Replace lines 94-104:
```lua
now_if_args(function()
  add('neovim/nvim-lspconfig')

  -- Use `:h vim.lsp.enable()` to automatically enable language server based on
  -- the rules provided by 'nvim-lspconfig'.
  -- Use `:h vim.lsp.config()` or 'after/lsp/' directory to configure servers.
  -- Uncomment and tweak the following `vim.lsp.enable()` call to enable servers.
  -- vim.lsp.enable({
  --   -- For example, if `lua-language-server` is installed, use `'lua_ls'` entry
  -- })
end)
```
With:
```lua
now_if_args(function()
  add('neovim/nvim-lspconfig')

  vim.lsp.config('bashls', {
    filetypes = { 'bash', 'sh', 'zsh' },
  })

  vim.lsp.enable({
    'lua_ls',
    'ty',
    'ruff',
    'ts_ls',
    'gopls',
    'bashls',
    'rust_analyzer',
    'actionlint',
    'gh-actions-language-server',
    'yaml-language-server',
  })
end)
```

- [ ] **Step 3: Verify**

Open a Lua file: `nvim plugin/40_plugins.lua`
Run `:LspInfo` — should show lua_ls attached (if installed via Mason).
Run `:Mason` — should open the Mason UI.

- [ ] **Step 4: Commit**

```bash
git add plugin/40_plugins.lua
git commit -m "Enable Mason and configure LSP servers"
```

---

### Task 4: Expand treesitter languages and add treesitter-context

**Files:**
- Modify: `plugin/40_plugins.lua:40-77` (expand languages, add context plugin)

- [ ] **Step 1: Expand the languages table and add treesitter-context**

Replace lines 40-77 (the entire `now_if_args` treesitter block):
```lua
now_if_args(function()
  add({
    source = 'nvim-treesitter/nvim-treesitter',
    -- Update tree-sitter parser after plugin is updated
    hooks = { post_checkout = function() vim.cmd('TSUpdate') end },
  })
  add('nvim-treesitter/nvim-treesitter-textobjects')

  -- Define languages which will have parsers installed and auto enabled
  -- After changing this, restart Neovim once to install necessary parsers. Wait
  -- for the installation to finish before opening a file for added language(s).
  local languages = {
    -- These are already pre-installed with Neovim. Used as an example.
    'lua',
    'vimdoc',
    'markdown',
    -- Add here more languages with which you want to use tree-sitter
    -- To see available languages:
    -- - Execute `:=require('nvim-treesitter').get_available()`
    -- - Visit 'SUPPORTED_LANGUAGES.md' file at
    --   https://github.com/nvim-treesitter/nvim-treesitter
  }
  local isnt_installed = function(lang)
    return #vim.api.nvim_get_runtime_file('parser/' .. lang .. '.*', false) == 0
  end
  local to_install = vim.tbl_filter(isnt_installed, languages)
  if #to_install > 0 then require('nvim-treesitter').install(to_install) end

  -- Enable tree-sitter after opening a file for a target language
  local filetypes = {}
  for _, lang in ipairs(languages) do
    for _, ft in ipairs(vim.treesitter.language.get_filetypes(lang)) do
      table.insert(filetypes, ft)
    end
  end
  local ts_start = function(ev) vim.treesitter.start(ev.buf) end
  Config.new_autocmd('FileType', filetypes, ts_start, 'Start tree-sitter')
end)
```
With:
```lua
now_if_args(function()
  add({
    source = 'nvim-treesitter/nvim-treesitter',
    hooks = { post_checkout = function() vim.cmd('TSUpdate') end },
  })
  add('nvim-treesitter/nvim-treesitter-textobjects')
  add('nvim-treesitter/nvim-treesitter-context')

  local languages = {
    'lua', 'vimdoc', 'markdown', 'markdown_inline',
    'python', 'typescript', 'javascript', 'tsx',
    'go', 'rust', 'bash', 'yaml', 'typst',
    'html', 'css', 'json', 'toml', 'diff',
    'c', 'luadoc', 'query',
  }
  local isnt_installed = function(lang)
    return #vim.api.nvim_get_runtime_file('parser/' .. lang .. '.*', false) == 0
  end
  local to_install = vim.tbl_filter(isnt_installed, languages)
  if #to_install > 0 then require('nvim-treesitter').install(to_install) end

  local filetypes = {}
  for _, lang in ipairs(languages) do
    for _, ft in ipairs(vim.treesitter.language.get_filetypes(lang)) do
      table.insert(filetypes, ft)
    end
  end
  local ts_start = function(ev) vim.treesitter.start(ev.buf) end
  Config.new_autocmd('FileType', filetypes, ts_start, 'Start tree-sitter')

  -- Sticky context header showing current function/class at window top
  require('treesitter-context').setup()
end)
```

- [ ] **Step 2: Verify**

Open a Lua file with functions: `nvim plugin/30_mini.lua`
Scroll into a function body — treesitter-context should show the function signature pinned at top.
First launch will install new parsers — wait for it to finish.

- [ ] **Step 3: Commit**

```bash
git add plugin/40_plugins.lua
git commit -m "Expand treesitter languages and add treesitter-context"
```

---

### Task 5: Add LazyGit

**Files:**
- Modify: `plugin/40_plugins.lua` (add lazygit section after snippets block)
- Modify: `plugin/20_keymaps.lua` (add `<Leader>gg` keymap)

- [ ] **Step 1: Add lazygit plugin to 40_plugins.lua**

After the snippets block (after line 141), add:
```lua

-- LazyGit =====================================================================
later(function()
  add('kdheepak/lazygit.nvim')
end)
```

- [ ] **Step 2: Add keymap to 20_keymaps.lua**

After the existing `<Leader>g` git keymaps block (after `xmap_leader('gs', ...)`), add:
```lua
nmap_leader('gg', '<Cmd>LazyGit<CR>',                         'LazyGit')
```

- [ ] **Step 3: Verify**

Open nvim, press `<Space>gg` — LazyGit should open (requires `lazygit` binary installed).

- [ ] **Step 4: Commit**

```bash
git add plugin/40_plugins.lua plugin/20_keymaps.lua
git commit -m "Add LazyGit integration"
```

---

### Task 6: Add Trouble.nvim

**Files:**
- Modify: `plugin/40_plugins.lua` (add trouble section)
- Modify: `plugin/20_keymaps.lua` (add `<Leader>x` group + clue)

- [ ] **Step 1: Add trouble plugin to 40_plugins.lua**

After the LazyGit block, add:
```lua

-- Trouble =====================================================================
later(function()
  add('folke/trouble.nvim')
  require('trouble').setup()
end)
```

- [ ] **Step 2: Add clue group to 20_keymaps.lua**

In the `Config.leader_group_clues` table, add:
```lua
  { mode = 'n', keys = '<Leader>x', desc = '+Trouble' },
```

- [ ] **Step 3: Add keymaps to 20_keymaps.lua**

After the `v` (Visits) keymap section, before `-- stylua: ignore end`, add:
```lua

-- x is for 'Trouble'. Common usage:
-- - `<Leader>xx` - toggle workspace diagnostics
-- - `<Leader>xt` - toggle TODO list
nmap_leader('xx', '<Cmd>Trouble diagnostics toggle<CR>',                    'Diagnostics')
nmap_leader('xX', '<Cmd>Trouble diagnostics toggle filter.buf=0<CR>',      'Diagnostics (buf)')
nmap_leader('xq', '<Cmd>Trouble qflist toggle<CR>',                        'Quickfix')
nmap_leader('xl', '<Cmd>Trouble loclist toggle<CR>',                       'Location list')
nmap_leader('xt', '<Cmd>Trouble todo toggle<CR>',                          'TODOs')
nmap_leader('xs', '<Cmd>Trouble symbols toggle focus=false<CR>',           'Symbols')
```

- [ ] **Step 4: Verify**

Open nvim, press `<Space>xx` — Trouble diagnostics panel should toggle.
Press `<Space>` and wait — mini.clue should show the `x` group as "+Trouble".

- [ ] **Step 5: Commit**

```bash
git add plugin/40_plugins.lua plugin/20_keymaps.lua
git commit -m "Add Trouble.nvim for diagnostics panel"
```

---

### Task 7: Add Grug-far

**Files:**
- Modify: `plugin/40_plugins.lua` (add grug-far section)
- Modify: `plugin/20_keymaps.lua` (add `<Leader>r` group + clue)

- [ ] **Step 1: Add grug-far plugin to 40_plugins.lua**

After the Trouble block, add:
```lua

-- Grug-far ====================================================================
later(function()
  add('MagicDuck/grug-far.nvim')
  require('grug-far').setup()
end)
```

- [ ] **Step 2: Add clue group to 20_keymaps.lua**

In the `Config.leader_group_clues` table, add:
```lua
  { mode = 'n', keys = '<Leader>r', desc = '+Replace' },
```

- [ ] **Step 3: Add keymaps to 20_keymaps.lua**

After the Trouble keymap section, before `-- stylua: ignore end`, add:
```lua

-- r is for 'Replace'. Common usage:
-- - `<Leader>rr` - search and replace current word
nmap_leader('rr', function()
  require('grug-far').open({ prefills = { search = vim.fn.expand('<cword>') } })
end, 'Search and replace')
```

- [ ] **Step 4: Verify**

Open nvim, place cursor on a word, press `<Space>rr` — Grug-far should open with cursor word prefilled.

- [ ] **Step 5: Commit**

```bash
git add plugin/40_plugins.lua plugin/20_keymaps.lua
git commit -m "Add Grug-far for search and replace"
```
