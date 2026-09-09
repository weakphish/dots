-- Obsidian vault navigation and note creation.

Config.later(function()
	vim.pack.add({
		{
			src = "https://github.com/obsidian-nvim/obsidian.nvim",
			version = vim.version.range("*"),
		},
	})

	require("obsidian").setup({
		legacy_commands = false,
		workspaces = {
			{ name = "vault", path = "~/vault" },
		},
		new_notes_location = "notes_subdir",
		notes_subdir = "notes",
		daily_notes = {
			folder = "daily",
			date_format = "YYYY/MM/YYYY-MM-DD",
		},
	})

end)
