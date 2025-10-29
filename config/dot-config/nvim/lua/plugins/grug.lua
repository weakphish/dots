return {
	"MagicDuck/grug-far.nvim",
	-- Note (lazy loading): grug-far.lua defers all it's requires so it's lazy by default
	-- additional lazy config to defer loading is not really needed...
	keys = {
		{
			"<leader>sar",
			'<cmd>:lua require("grug-far").open({ prefills = { search = vim.fn.expand("<cword>") } })<CR>',
			desc = "Search And Replace",
		},
	},
}
