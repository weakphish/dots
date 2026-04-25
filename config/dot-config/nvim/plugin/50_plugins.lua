-- Plugins outside of MINI

local add = vim.pack.add
local now_if_args, later = Config.now_if_args, Config.later

-- Tree-sitter ================================================================
now_if_args(function()
	local ts_update = function()
		vim.cmd("TSUpdate")
	end
	Config.on_packchanged("nvim-treesitter", { "update" }, ts_update, ":TSUpdate")

	add({
		"https://github.com/nvim-treesitter/nvim-treesitter",
		"https://github.com/nvim-treesitter/nvim-treesitter-textobjects",
		"https://github.com/nvim-treesitter/nvim-treesitter-context",
	})

	local languages = {
		"lua",
		"vimdoc",
		"markdown",
		"markdown_inline",
		"bash",
		"c",
		"diff",
		"html",
		"luadoc",
		"python",
		"go",
		"typescript",
		"jsdoc",
		"query",
		"vim",
		"yaml",
		"helm",
	}
	local isnt_installed = function(lang)
		return #vim.api.nvim_get_runtime_file("parser/" .. lang .. ".*", false) == 0
	end
	local to_install = vim.tbl_filter(isnt_installed, languages)
	if #to_install > 0 then
		require("nvim-treesitter").install(to_install)
	end

	local filetypes = {}
	for _, lang in ipairs(languages) do
		for _, ft in ipairs(vim.treesitter.language.get_filetypes(lang)) do
			table.insert(filetypes, ft)
		end
	end
	local ts_start = function(ev)
		vim.treesitter.start(ev.buf)
	end
	Config.new_autocmd("FileType", filetypes, ts_start, "Start tree-sitter")
end)

-- Language servers ============================================================
now_if_args(function()
	add({
		"https://github.com/neovim/nvim-lspconfig",
		"https://github.com/qvalentin/helm-ls.nvim",
	})

	require("helm-ls").setup()

	vim.lsp.enable({
		"ty",
		"ruff",
		"ts_ls",
		"gopls",
		"bashls",
		"rust_analyzer",
		"actionlint",
		"gh-actions-language-server",
		"yaml-language-server",
		"helm_ls",
		"lua_ls",
	})
end)

-- Mason (LSP/formatter installer) ============================================
now_if_args(function()
	add({ "https://github.com/mason-org/mason.nvim" })
	require("mason").setup()
end)

-- Formatting =================================================================
later(function()
	add({ "https://github.com/stevearc/conform.nvim" })

	require("conform").setup({
		default_format_opts = {
			lsp_format = "fallback",
		},
		formatters_by_ft = {
			lua = { "stylua" },
			python = { "isort", "black" },
			rust = { "rustfmt", lsp_format = "fallback" },
			json = { "prettierd", "prettier", stop_after_first = true },
			jsonc = { "prettierd", "prettier", stop_after_first = true },
			javascript = { "prettierd", "prettier", stop_after_first = true },
			typescript = { "prettierd", "prettier", stop_after_first = true },
			yaml = { "prettierd", "prettier", stop_after_first = true },
			go = { "gofmt" },
		},
		format_on_save = {
			timeout_ms = 500,
			lsp_format = "fallback",
		},
	})
end)

-- Snippets ===================================================================
later(function()
	add({ "https://github.com/rafamadriz/friendly-snippets" })
end)

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

-- LazyGit ====================================================================
later(function()
	add({ "https://github.com/kdheepak/lazygit.nvim" })
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

	require("ibl").setup({ indent = { highlight = highlight, char = "│" } })
end)

-- Grug-far (search and replace) ==============================================
later(function()
	add({ "https://github.com/MagicDuck/grug-far.nvim" })
end)
