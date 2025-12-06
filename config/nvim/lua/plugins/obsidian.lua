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
		daily_notes = {
			-- Optional, if you keep daily notes in a separate directory.
			folder = "daily",
		},
		completion = {
			-- Enables completion using nvim_cmp
			nvim_cmp = false,
			-- Enables completion using blink.cmp
			blink = true,
		},
		picker = {
			-- Set your preferred picker. Can be one of 'telescope.nvim', 'fzf-lua', 'mini.pick' or 'snacks.pick'.
			name = "fzf-lua",
		},
	},
	keys = {
		{ "<leader>ob", ":Obsidian backlinks<CR>", desc = "Backlinks" },
		{ "<leader>od", ":Obsidian today<CR>", desc = "Today's Daily Note" },
		{ "<leader>oD", ":Obsidian tomorrow<CR>", desc = "Tomorrow's Daily Note" },
		{ "<leader>ol", ":Obsidian links<CR>", desc = "Links In Note" },
		{ "<leader>ot", ":Obsidian search<CR>", desc = "Search Notes" },
		{ "<leader>ot", ":Obsidian tags<CR>", desc = "Tags" },
		{ "<leader>oy", ":Obsidian yesterday<CR>", desc = "Yesterday's Daily Note" },
	},
}
