-- Built-in Neovim behavior

-- stylua: ignore start

-- General ====================================================================
vim.g.mapleader = ' '

vim.o.mouse       = 'a'
vim.o.mousescroll = 'ver:25,hor:6'
vim.o.switchbuf   = 'usetab'
vim.o.undofile    = true

vim.o.shada = "'100,<50,s10,:1000,/100,@100,h"

vim.cmd('filetype plugin indent on')
if vim.fn.exists('syntax_on') ~= 1 then vim.cmd('syntax enable') end

-- UI =========================================================================
vim.o.breakindent    = true
vim.o.breakindentopt = 'list:-1'
vim.o.colorcolumn    = '+1'
vim.o.cursorline     = true
vim.o.linebreak      = true
vim.o.list           = true
vim.o.number         = true
vim.o.relativenumber = true
vim.o.pumborder      = 'single'
vim.o.pumheight      = 10
vim.o.pummaxwidth    = 100
vim.o.ruler          = false
vim.o.shortmess      = 'CFOSWaco'
vim.o.showmode       = false
vim.o.signcolumn     = 'yes'
vim.o.splitbelow     = true
vim.o.splitkeep      = 'screen'
vim.o.splitright     = true
vim.o.winborder      = 'single'
vim.o.wrap           = false

vim.o.cursorlineopt  = 'screenline,number'

vim.o.fillchars = 'eob: ,fold:╌'
vim.o.listchars = 'extends:…,nbsp:␣,precedes:…,tab:│ '

-- Folds
vim.o.foldlevel   = 10
vim.o.foldmethod  = 'indent'
vim.o.foldnestmax = 10
vim.o.foldtext    = ''

-- Editing ====================================================================
vim.o.autoindent    = true
vim.o.expandtab     = true
vim.o.formatoptions = 'rqnl1j'
vim.o.ignorecase    = true
vim.o.incsearch     = true
vim.o.infercase     = true
vim.o.shiftwidth    = 2
vim.o.smartcase     = true
vim.o.smartindent   = true
vim.o.spelloptions  = 'camel'
vim.o.tabstop       = 2
vim.o.virtualedit   = 'block'

vim.o.iskeyword = '@,48-57,_,192-255,-'

vim.o.formatlistpat = [[^\s*[0-9\-\+\*]\+[\.\)]*\s\+]]

-- Built-in completion
vim.o.complete        = '.,w,b,kspell'
vim.o.completeopt     = 'menuone,noselect,fuzzy,nosort'
vim.o.completetimeout = 100

-- Filetype additions =========================================================
vim.filetype.add({ filename = { ['~/.zshrc'] = 'sh' } })

-- Diagnostics ================================================================
local diagnostic_opts = {
  signs = { priority = 9999, severity = { min = 'WARN', max = 'ERROR' } },
  underline = { severity = { min = 'HINT', max = 'ERROR' } },
  virtual_lines = false,
  virtual_text = {
    current_line = true,
    severity = { min = 'ERROR', max = 'ERROR' },
  },
  update_in_insert = false,
}

Config.later(function() vim.diagnostic.config(diagnostic_opts) end)
-- stylua: ignore end
