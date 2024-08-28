return {
	-- Highly experimental plugin that completely replaces the UI for messages, cmdline and the popupmenu.
	{
		"folke/noice.nvim",
		event = "VeryLazy",
		opts = {
			lsp = {
				hover = {
					-- Set not show a message if hover is not available
					-- ex: shift+k on Typescript code
					silent = true,
				},
			},
		},
		dependencies = {
			-- if you lazy-load any plugin below, make sure to add proper `module="..."` entries
			"MunifTanjim/nui.nvim",
			-- OPTIONAL:
			--   `nvim-notify` is only needed, if you want to use the notification view.
			--   If not available, we use `mini` as the fallback
			"rcarriga/nvim-notify",
		},
		config = function()
			require("noice").setup({
				lsp = {
					-- override markdown rendering so that **cmp** and other plugins use **Treesitter**
					override = {
						["vim.lsp.util.convert_input_to_markdown_lines"] = true,
						["vim.lsp.util.stylize_markdown"] = true,
						["cmp.entry.get_documentation"] = true,
					},
				},
				-- you can enable a preset for easier configuration
				presets = {
					bottom_search = true, -- use a classic bottom cmdline for search
					command_palette = true, -- position the cmdline and popupmenu together
					long_message_to_split = true, -- long messages will be sent to a split
					inc_rename = true, -- enables an input dialog for inc-rename.nvim
					lsp_doc_border = true, -- add a border to hover docs and signature help
				},
			})
		end,
	},

	-- Symbol tree
	{
		"stevearc/aerial.nvim",
		opts = {},
		-- Optional dependencies
		dependencies = {
			"nvim-treesitter/nvim-treesitter",
			"nvim-tree/nvim-web-devicons",
		},
	},

	-- Dashboard like Doom Emacs
	{
		"goolord/alpha-nvim",
		event = "VimEnter",
		dependencies = { "nvim-tree/nvim-web-devicons" },
		config = function()
			local alpha = require("alpha")
			local dashboard = require("alpha.themes.dashboard")

			dashboard.section.header.val = "weakphish"
			dashboard.section.buttons.val = {
				dashboard.button("e", "  New file", ":ene <BAR> startinsert <CR>"),
				dashboard.button("q", "  Quit NVIM", ":qa<CR>"),
			}

			-- Read a fortune :)
			local handle = io.popen("fortune")
			local fortune = handle:read("*a")
			handle:close()
			dashboard.section.footer.val = fortune
			alpha.setup(dashboard.opts)
		end,
	},

	-- Pretty Icons
	"nvim-tree/nvim-web-devicons",

	-- Highlight the current symbol in the buffer
	"RRethy/vim-illuminate",

	-- Detect tabstop and shiftwidth automatically
	"tpope/vim-sleuth",

	{
		-- Neotree - file tree browser
		"nvim-neo-tree/neo-tree.nvim",
		dependencies = {
			"nvim-lua/plenary.nvim",
			"nvim-tree/nvim-web-devicons", -- not strictly required, but recommended
			"MunifTanjim/nui.nvim",
		},
	},

	{
		-- Adds git releated signs to the gutter, as well as utilities for managing changes
		"lewis6991/gitsigns.nvim",
		config = function()
			require("gitsigns").setup()
		end,
	},

	-- Pretty colors
	{
		"neanias/everforest-nvim",
		version = false,
		lazy = false,
		priority = 1000, -- make sure to load this before all the other start plugins
		-- Optional; default configuration will be used if setup isn't called.
		config = function()
			require("everforest").setup({
				-- Your config here
			})
			require("everforest").load()
		end,
	},

	{
		-- Set lualine as statusline
		"nvim-lualine/lualine.nvim",
		-- See `:help lualine.txt`
		opts = {
			options = {
				theme = "everforest",
				icons_enabled = false,
				component_separators = "|",
				section_separators = "",
			},
		},
		config = function()
			require("lualine").setup({ sections = { lualine_x = { "aerial" } } })
		end,
	},

	{
		-- Tabs, but less bad and more good
		"akinsho/bufferline.nvim",
		dependencies = "nvim-tree/nvim-web-devicons",
		config = function()
			require("bufferline").setup({
				options = {
					offsets = {
						{
							filetype = "neo-tree",
							text = "File Explorer",
							text_align = "center",
						},
					},
				},
			})
		end,
	},

	{
		-- Indent guides
		"lukas-reineke/indent-blankline.nvim",
		main = "ibl",
		opts = {},
		config = function()
			require("ibl").setup()
		end,
	},

	{
		-- Show current context in winbar
		"utilyre/barbecue.nvim",
		name = "barbecue",
		version = "*",
		dependencies = {
			"SmiteshP/nvim-navic",
			"nvim-tree/nvim-web-devicons", -- optional dependency
		},
		opts = {
			-- configurations go here
		},
	},

	-- VS Code like toggle term
	{'akinsho/toggleterm.nvim', version = "*", config = true}
}
