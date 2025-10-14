--  A snazzy bufferline for Neovim
return {
	"akinsho/bufferline.nvim",
	version = "*",
	lazy = false,
	dependencies = "nvim-tree/nvim-web-devicons",
	keys = {
		{
			"<leader>bo",
			"<cmd>BufferLinePick<CR>",
			desc = "[O]pen Buffer",
		},
		{
			"<leader>bn",
			"<cmd>BufferLineCycleNext<CR>",
			desc = "[N]ext Buffer",
		},
		{
			"<leader>bp",
			"<cmd>BufferLineCyclePrev<CR>",
			desc = "[P]revious Buffer",
		},
	},
	config = function()
		require("bufferline").setup({})
	end,
}
