-- Bootstrap Lazy package manager, which will load plugins in lua/plugin
require("config.lazy_init")

-- Load general vim config
require("config.options")
require("config.autocmds")
require("config.keymaps")

-- Load LSP
vim.lsp.enable("basedpyright")
vim.lsp.config("basedpyright", {
	-- Override type checking mode, and do full workspace diagnostics
	settings = {
		basedpyright = {
			analysis = {
				typeCheckingMode = "standard",
				diagnosticMode = "workspace",
			},
		},
	},
})
vim.lsp.config("bashls", {
	filetypes = {"bash", "sh", ".zshrc"}
})

vim.lsp.enable("ruff")
vim.lsp.enable("ts_ls")
vim.lsp.enable("gopls")
vim.lsp.enable("bashls")
