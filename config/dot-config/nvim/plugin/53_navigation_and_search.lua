-- Navigation, diagnostics lists, and project-wide search/replace.

local add = vim.pack.add
local later = Config.later

-- Flash ======================================================================
later(function()
	add({ "https://github.com/folke/flash.nvim" })

	require("flash").setup({
		label = {
			rainbow = {
				enabled = true,
			},
		},
	})
end)

-- Trouble ====================================================================
later(function()
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
later(function()
	add({ "https://github.com/MagicDuck/grug-far.nvim" })
end)
