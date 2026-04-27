-- Git integrations.

local add = vim.pack.add
local later = Config.later

-- LazyGit ====================================================================
later(function()
	add({ "https://github.com/kdheepak/lazygit.nvim" })
end)
