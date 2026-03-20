-- Bootstrap Lazy package manager, which will load plugins in lua/plugin
require("config.lazy_init")

-- Load general vim config
require("config.options")
require("config.autocmds")
require("config.keymaps")

-- Load LSP
-- vim.lsp.enable("basedpyright")
-- vim.lsp.config("basedpyright", {
-- 	-- Override type checking mode, and do full workspace diagnostics
-- 	settings = {
-- 		basedpyright = {
-- 			analysis = {
-- 				typeCheckingMode = "standard",
-- 				diagnosticMode = "workspace",
-- 			},
-- 		},
-- 	},
-- })
vim.lsp.enable("ty")
vim.lsp.config("bashls", {
	filetypes = {"bash", "sh", "zsh"}
})

vim.lsp.config("ruff", {
	on_attach = function(client)
		client.server_capabilities.documentFormattingProvider = false
		client.server_capabilities.documentRangeFormattingProvider = false
	end,
})
vim.lsp.enable("ruff")
vim.lsp.enable("ts_ls")
vim.lsp.enable("gopls")
vim.lsp.enable("bashls")
vim.lsp.enable("rust_analyzer")
vim.lsp.enable("actionlint")
vim.lsp.enable("gh-actions-language-server")
vim.lsp.enable("yaml-language-server")
