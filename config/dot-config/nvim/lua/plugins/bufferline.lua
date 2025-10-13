--  A snazzy bufferline for Neovim
return {
	"akinsho/bufferline.nvim",
	version = "*",
	dependencies = "nvim-tree/nvim-web-devicons",
	config = function()
		vim.opt.termguicolors = true
		require("bufferline").setup({})
		vim.keymap.set("n", "<leader>bo", "<cmd>BufferLinePick<CR>", { desc = "Open Buffer", remap = true })
	end,
}
