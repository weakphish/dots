-- Autocommands ===============================================================

-- Don't auto-wrap comments and don't insert comment leader after hitting 'o'
local f = function()
	vim.cmd("setlocal formatoptions-=c formatoptions-=o")
end
Config.new_autocmd("FileType", nil, f, "Proper 'formatoptions'")

-- Highlight when yanking text
Config.new_autocmd("TextYankPost", nil, function()
	vim.hl.on_yank()
end, "Highlight on yank")
