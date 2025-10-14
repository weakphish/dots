-- Provide server-specific default configs on top of built-in LSP client
-- Can be customize and merge user-configs in lua/lsp/
return {
	"neovim/nvim-lspconfig",
	lazy = false,
	keys = {
		{
			"<leader>ca",
			"<cmd>lua vim.lsp.buf.code_action()<CR>",
			desc = "Code [a]ction",
		},
		{
			"<leader>cr",
			"<cmd>lua vim.lsp.buf.rename()<CR>",
			desc = "[R]ename",
		},
		{
			"<leader>cy",
			"<cmd>lua vim.lsp.buf.type_definition()<CR>",
			desc = "T[y]pe definition",
		},
		{
			"<leader>ci",
			"<cmd>lua vim.lsp.buf.implementation()<CR>",
			desc = "[I]mplementation",
		},
		{
			"<leader>ce",
			"<cmd>lua vim.lsp.buf.references()<CR>",
			desc = "R[e]ferences",
		},
	},
}
