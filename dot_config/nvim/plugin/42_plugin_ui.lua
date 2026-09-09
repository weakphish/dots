-- Visual polish and editor UI helpers.

local add = vim.pack.add
local now, later = Config.now, Config.later

-- Colorscheme ================================================================
now(function()
	add({ "https://github.com/ellisonleao/gruvbox.nvim" })
	vim.o.background = "dark"
	vim.cmd("colorscheme gruvbox")
end)
-- now(function()
-- 	add({ "https://github.com/neanias/everforest-nvim.git" })
-- 	vim.o.background = "dark"
-- 	require("everforest").setup({
-- 		---Controls the "hardness" of the background. Options are "soft", "medium" or "hard".
-- 		---Default is "medium".
-- 		background = "hard",
-- 	})
-- 	require("everforest").load()
-- end)

-- Bufferline =================================================================
now(function()
	add({ "https://github.com/akinsho/bufferline.nvim" })
	require("bufferline").setup()
end)

-- Statusline =================================================================
now(function()
	add({
		"https://github.com/nvim-tree/nvim-web-devicons",
		"https://github.com/nvim-lualine/lualine.nvim",
	})
	require("lualine").setup({
		options = { theme = "gruvbox" },
	})
end)


-- Rainbow indent guides ======================================================
later(function()
	add({ "https://github.com/lukas-reineke/indent-blankline.nvim" })

	local function dim_color(hex, factor)
		factor = factor or 0.4
		local r = math.floor(tonumber(hex:sub(2, 3), 16) * factor)
		local g = math.floor(tonumber(hex:sub(4, 5), 16) * factor)
		local b = math.floor(tonumber(hex:sub(6, 7), 16) * factor)
		return string.format("#%02x%02x%02x", r, g, b)
	end

	local function get_highlight_color(name)
		local hl = vim.api.nvim_get_hl(0, { name = name })
		if hl.fg then
			return string.format("#%06x", hl.fg)
		end
		return nil
	end

	local everforest_groups = {
		{ source = "Red", target = "IndentRed" },
		{ source = "Yellow", target = "IndentYellow" },
		{ source = "Blue", target = "IndentBlue" },
		{ source = "Orange", target = "IndentOrange" },
		{ source = "Green", target = "IndentGreen" },
		{ source = "Purple", target = "IndentPurple" },
		{ source = "Aqua", target = "IndentAqua" },
	}
	local gruvbox_groups = {
		{ source = "GruvboxRed", target = "IndentRed" },
		{ source = "GruvboxYellow", target = "IndentYellow" },
		{ source = "GruvboxBlue", target = "IndentBlue" },
		{ source = "GruvboxOrange", target = "IndentOrange" },
		{ source = "GruvboxGreen", target = "IndentGreen" },
		{ source = "GruvboxPurple", target = "IndentPurple" },
		{ source = "GruvboxAqua", target = "IndentAqua" },
	}

	local highlight = {}
	for _, group in ipairs(gruvbox_groups) do
		table.insert(highlight, group.target)
	end

	local hooks = require("ibl.hooks")
	hooks.register(hooks.type.HIGHLIGHT_SETUP, function()
		for _, group in ipairs(gruvbox_groups) do
			local color = get_highlight_color(group.source)
			if color then
				vim.api.nvim_set_hl(0, group.target, { fg = dim_color(color, 0.4) })
			end
		end
	end)

	require("ibl").setup({ indent = { highlight = highlight, char = "│" } })
end)

-- Rainbow brackets
now(function()
	add({ "https://github.com/hiphish/rainbow-delimiters.nvim" })
	require("rainbow-delimiters.setup").setup()
end)
