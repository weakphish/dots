-- Navigation, diagnostics lists, and project-wide search/replace.

local add = vim.pack.add
local now = Config.now

-- Flash ======================================================================
now(function()
	add({ "https://github.com/folke/flash.nvim" })

	require("flash").setup({
		label = {
			rainbow = {
				enabled = true,
			},
		},
	})
end)

-- Fzf-lua ====================================================================
now(function()
	add({ "https://github.com/ibhagwan/fzf-lua" }, { load = true })

	require("fzf-lua").setup({
		ui_select = true,
	})
end)

-- Trouble ====================================================================
now(function()
	add({ "https://github.com/folke/trouble.nvim" })

	require("trouble").setup({
		modes = {
			symbols = {
				win = {
					type = "split",
					relative = "win",
					position = "right",
					size = 0.15,
				},
			},
		},
	})
end)

-- Grug-far (search and replace) ==============================================
now(function()
	add({ "https://github.com/MagicDuck/grug-far.nvim" })
end)
