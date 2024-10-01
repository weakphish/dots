-- Prevent flickering inside Zellij
vim.opt.termsync = true

-- Set <space> as the leader key
  -- See `:help mapleader`
  --  NOTE: Must happen before plugins are required (otherwise wrong leader will be used)
  vim.g.mapleader = ' '
  vim.g.maplocalleader = ' '

  -- [[ Setting general options ]]
  -- Set highlight on search
  vim.o.hlsearch = false
  
  -- Use 4 spaces for indentation
  vim.o.tabstop = 4 -- A TAB character looks like 4 spaces
  vim.o.softtabstop = 4 -- Number of spaces inserted instead of a TAB character
  vim.o.shiftwidth = 4 -- Number of spaces inserted when indenting
  vim.o.expandtab = true -- Pressing the TAB key will insert spaces instead of a TAB character

  -- Make line numbers default
  vim.wo.number = true
  vim.wo.relativenumber = true

  -- Add a ruler (color column) at 120
  vim.opt.colorcolumn = '120'

  -- Enable mouse mode
  vim.o.mouse = 'a'

  -- Enable break indent
  vim.o.breakindent = true

  -- Save undo history
  vim.o.undofile = true

  -- Case insensitive searching UNLESS /C or capital in search
  vim.o.ignorecase = true
  vim.o.smartcase = true

  -- Keep signcolumn on by default
  vim.wo.signcolumn = 'yes'

  -- Decrease update time
  vim.o.updatetime = 250
  vim.o.timeout = true
  vim.o.timeoutlen = 300

  -- Set completeopt to have a better completion experience
  vim.o.completeopt = 'menuone,noselect'

  -- NOTE: You should make sure your terminal supports this
  vim.o.termguicolors = true

  vim.o.guifont = 'JetBrains Mono:h14' -- text below applies for VimScript

  -- [[ Basic Keymaps ]]
  -- Keymaps for better default experience
  -- See `:help vim.keymap.set()`
  vim.keymap.set({ 'n', 'v' }, '<Space>', '<Nop>', { silent = true })

  -- Remap for dealing with word wrap
  vim.keymap.set('n', 'k', "v:count == 0 ? 'gk' : 'k'", { expr = true, silent = true })
  vim.keymap.set('n', 'j', "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })

  -- [[ Highlight on yank ]]
  -- See `:help vim.highlight.on_yank()`
  local highlight_group = vim.api.nvim_create_augroup('YankHighlight', { clear = true })
  vim.api.nvim_create_autocmd('TextYankPost', {
    callback = function()
      vim.highlight.on_yank()
    end,
    group = highlight_group,
    pattern = '*',
  })

  -- [[ Filetype Detection ]]
  -- Detect Jenkinsfile as a Groovy Filetype
  vim.filetype.add {
    filename = {
      ['Jenkinsfile'] = 'groovy',
      ['jenkinsfile'] = 'groovy',
    },
  }

-- Enable inlay hints
vim.lsp.inlay_hint.enable(true)

