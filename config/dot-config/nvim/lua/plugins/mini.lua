-- Collection of various small independent plugins/modules
return {
	{
		"echasnovski/mini.nvim",
		config = function()
			-- Better Around/Inside textobjects
			--
			-- Examples:
			--  - va)  - [V]isually select [A]round [)]paren
			--  - yinq - [Y]ank [I]nside [N]ext [Q]uote
			--  - ci'  - [C]hange [I]nside [']quote
			require("mini.ai").setup({ n_lines = 500 })

			-- Add/delete/replace surroundings (brackets, quotes, etc.)
			--
			-- - saiw) - [S]urround [A]dd [I]nner [W]ord [)]Paren
			-- - sd'   - [S]urround [D]elete [']quotes
			-- - sr)'  - [S]urround [R]eplace [)] [']
			require("mini.surround").setup({
				-- Module mappings. Use `''` (empty string) to disable one.
				mappings = {
					add = "gsa", -- Add surrounding in Normal and Visual modes
					delete = "gsd", -- Delete surrounding
					find = "gsf", -- Find surrounding (to the right)
					find_left = "gsF", -- Find surrounding (to the left)
					highlight = "gsh", -- Highlight surrounding
					replace = "gsr", -- Replace surrounding

					suffix_last = "l", -- Suffix to search with "prev" method
					suffix_next = "n", -- Suffix to search with "next" method
				},
			})

			-- Simple and easy statusline.
			--  You could remove this setup call if you don't like it,
			--  and try some other statusline plugin
			local statusline = require("mini.statusline")
			-- set use_icons to true if you have a Nerd Font
			statusline.setup({ use_icons = vim.g.have_nerd_font })

			-- You can configure sections in the statusline by overriding their
			-- default behavior. For example, here we set the section for
			-- cursor location to LINE:COLUMN
			---@diagnostic disable-next-line: duplicate-set-field
			statusline.section_location = function()
				return "%2l:%-2v"
			end

			-- Like which-key, but mini. Show keymap combos as you're doing them.
			local miniclue = require("mini.clue")
			miniclue.setup({
				triggers = {
					-- Leader triggers
					{ mode = "n", keys = "<Leader>" },
					{ mode = "x", keys = "<Leader>" },

					-- Built-in completion
					{ mode = "i", keys = "<C-x>" },

					-- `g` key
					{ mode = "n", keys = "g" },
					{ mode = "x", keys = "g" },

					-- Marks
					{ mode = "n", keys = "'" },
					{ mode = "n", keys = "`" },
					{ mode = "x", keys = "'" },
					{ mode = "x", keys = "`" },

					-- Registers
					{ mode = "n", keys = '"' },
					{ mode = "x", keys = '"' },
					{ mode = "i", keys = "<C-r>" },
					{ mode = "c", keys = "<C-r>" },

					-- Window commands
					{ mode = "n", keys = "<C-w>" },

					-- `z` key
					{ mode = "n", keys = "z" },
					{ mode = "x", keys = "z" },
				},

				clues = {
					-- Enhance this by adding descriptions for <Leader> mapping groups
					miniclue.gen_clues.builtin_completion(),
					miniclue.gen_clues.g(),
					miniclue.gen_clues.marks(),
					miniclue.gen_clues.registers(),
					miniclue.gen_clues.windows(),
					miniclue.gen_clues.z(),

					-- Add descriptions for mapping groups
					{ mode = "n", keys = "<Leader>b", desc = "+Buffers" },
					{ mode = "n", keys = "<Leader>c", desc = "+Code" },
					{ mode = "n", keys = "<Leader>f", desc = "+Files" },
					{ mode = "n", keys = "<Leader>l", desc = "+LSP" },
					{ mode = "n", keys = "<Leader>m", desc = "+Sessions" },
					{ mode = "n", keys = "<Leader>o", desc = "+Obsidian" },
					{ mode = "n", keys = "<Leader>s", desc = "+Search" },
					{ mode = "n", keys = "<Leader>x", desc = "+Trouble" },
				},
				-- Clue window settings
				window = {
					-- Delay before showing clue window
					delay = 500,
				},
			})

			-- File picker/general picker!
			require("mini.pick").setup()
			vim.keymap.set("n", "<Leader>bb", ":Pick buffers<CR>", { desc = "Pick buffer" })
			vim.keymap.set("n", "<Leader>ff", ":Pick files<CR>", { desc = "Pick file" })
			vim.keymap.set("n", "<Leader>fe", ":Pick explorer<CR>", { desc = "Explorer" })
			vim.keymap.set("n", "<Leader>sg", ":Pick grep_live<CR>", { desc = "Live Grep" })

			-- Add extra mini features, like more pickers
			require("mini.extra").setup()

			-- File explorer/manipulator
			require("mini.files").setup()
			vim.keymap.set("n", "<Leader>fn", ":lua MiniFiles.open()<CR>", { desc = "File Tree" })

			-- Autopairs pls
			require("mini.pairs").setup()

			-- Sessions!
			require("mini.sessions").setup()
			vim.keymap.set("n", "<Leader>ms", "<Cmd>lua MiniSessions.select()<CR>", { desc = "[S]elect session" })
			vim.keymap.set(
				"n",
				"<Leader>md",
				"<Cmd>lua MiniSessions.select('delete')<CR>",
				{ desc = "[D]elete session" }
			)
			vim.keymap.set(
				"n",
				"<leader>mw",
				'<Cmd>lua MiniSessions.write(vim.fn.input("Session Name > "))<CR>',
				{ desc = "[W]rite session" }
			)

			-- Indent lines
			require("mini.indentscope").setup()

			-- Notifications
			require("mini.notify").setup()
		end,
	},
}
