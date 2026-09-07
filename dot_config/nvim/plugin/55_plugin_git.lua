-- Git integrations.

local add = vim.pack.add
local now = Config.now

-- LazyGit ====================================================================
now(function()
	add({ "https://github.com/kdheepak/lazygit.nvim" })
end)

-- GitSigns for gutter/blame/etc in the UI
now(function()
	add({ "https://github.com/lewis6991/gitsigns.nvim" })
end)
