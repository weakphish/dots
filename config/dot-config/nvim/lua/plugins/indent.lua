return {
	"lukas-reineke/indent-blankline.nvim",
	main = "ibl",
	---@module "ibl"
	---@type ibl.config
	opts = {},
	config = function()
		local function dim_color(hex, factor)
			factor = factor or 0.4
			local r = math.floor(tonumber(hex:sub(2, 3), 16) * factor)
			local g = math.floor(tonumber(hex:sub(4, 5), 16) * factor)
			local b = math.floor(tonumber(hex:sub(6, 7), 16) * factor)
			return string.format("#%02x%02x%02x", r, g, b)
		end

		local function get_gruvbox_color(name)
			local hl = vim.api.nvim_get_hl(0, { name = name })
			if hl.fg then
				return string.format("#%06x", hl.fg)
			end
			return nil
		end

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
				local color = get_gruvbox_color(group.source)
				if color then
					vim.api.nvim_set_hl(0, group.target, { fg = dim_color(color, 0.4) })
				end
			end
		end)

		require("ibl").setup({ indent = { highlight = highlight } })
	end,
}
