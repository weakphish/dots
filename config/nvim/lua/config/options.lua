vim.g.mapleader = " " -- Set `<Leader>` before making any mappings and configuring 'mini.clue'

vim.opt.termguicolors = true

vim.opt.shiftwidth = 4 -- Size of an indent
vim.opt.tabstop = 4 -- size of a tab

-- Mostly for Flash's benefit. Ignore case unless an upper-case is added to search
vim.opt.ignorecase = true
vim.opt.smartcase = true

vim.opt.conceallevel = 1

vim.wo.number = true
vim.wo.relativenumber = true -- Relative line numbers

-- Diagnostics
vim.diagnostic.config({ virtual_text = true })
vim.keymap.set("n", "<Leader>cd", "<cmd>lua vim.diagnostic.open_float()<CR>", { desc = "Show [d]iagnostic" })
