-- Bootstrap Lazy package manager, which will load plugins in lua/plugin
require("config.lazy_init")

-- Load general vim config
require("config.options")
require("config.autocmds")

-- Load LSP
vim.lsp.enable("basedpyright")
vim.lsp.enable("ruff")
