-- Git integrations.

local add = vim.pack.add
local now = Config.now

-- LazyGit ====================================================================
now(function()
	add({ "https://github.com/kdheepak/lazygit.nvim" })
end)
