-- every spec file under the "plugins" directory will be loaded automatically by lazy.nvim
return {
	"ellisonleao/gruvbox.nvim", 
	priority = 1000 , 
	config = function ()
		vim.o.background = "dark" -- or "light" for light mode
		vim.cmd([[colorscheme gruvbox]])
	end,
	opts = ...
}
