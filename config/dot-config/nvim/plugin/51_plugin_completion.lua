-- Completion and signature help.

local add = vim.pack.add
local now_if_args = Config.now_if_args

now_if_args(function()
	add({
		{ src = "https://github.com/Saghen/blink.cmp", version = vim.version.range("1") },
	})

	local cmp = require("blink.cmp")
	cmp.setup({
		keymap = {
			preset = "enter",
			["<Tab>"] = { "select_next", "snippet_forward", "fallback" },
			["<S-Tab>"] = { "select_prev", "snippet_backward", "fallback" },
		},
		completion = {
			documentation = { auto_show = false },
		},
		snippets = {
			preset = "mini_snippets",
		},
		sources = {
			default = { "lsp", "path", "snippets", "buffer" },
		},
		fuzzy = {
			implementation = "lua",
		},
		signature = { enabled = true },
	})

	vim.lsp.config("*", { capabilities = cmp.get_lsp_capabilities() })
end)
