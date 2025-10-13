return {
	"obsidian-nvim/obsidian.nvim",
	version = "*", -- recommended, use latest release instead of latest commit
	ft = "markdown",
	---@module 'obsidian'
	---@type obsidian.config
	opts = {
		workspaces = {
			{
				name = "work",
				path = "~/vaults/work",
			},
		},
	},
	keys = {
		{ "<leader>od", ":Obsidian today<CR>", desc = "Today's Daily Note" },
		{ "<leader>ot", ":ObsidianTags<CR>", desc = "Tags" },
		{ "<leader>oy", ":ObsidianYesterday<CR>", desc = "Yesterday's Daily Note" },
	},
}
