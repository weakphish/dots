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
					{ mode = "n", keys = "<Leader>g", desc = "+Git" },
					{ mode = "n", keys = "<Leader>l", desc = "+LSP" },
					{ mode = "n", keys = "<Leader>m", desc = "+Sessions" },
					{ mode = "n", keys = "<Leader>o", desc = "+Obsidian" },
					{ mode = "n", keys = "<Leader>s", desc = "+Search" },
					{ mode = "n", keys = "<Leader>sa", desc = "+Search And Replace" },
					{ mode = "n", keys = "<Leader>x", desc = "+Trouble" },
				},
				-- Clue window settings
				window = {
					-- Delay before showing clue window
					delay = 500,
				},
			})

			-- -- File picker/general picker!
			-- require("mini.pick").setup()
			-- vim.keymap.set("n", "<Leader>bb", ":Pick buffers<CR>", { desc = "Pick buffer" })
			-- vim.keymap.set("n", "<Leader>ff", ":Pick files<CR>", { desc = "Pick file" })
			-- vim.keymap.set("n", "<Leader>fe", ":Pick explorer<CR>", { desc = "Explorer" })
			-- vim.keymap.set("n", "<Leader>sg", ":Pick grep_live<CR>", { desc = "Live Grep" })
			-- vim.keymap.set("n", "<Leader>ss", ":Pick lsp scope='document_symbol'", { desc = "Symbols" })
			--
			-- Add extra mini features, like more pickers
			-- require("mini.extra").setup()

			-- File explorer/manipulator
			require("mini.files").setup()
			vim.keymap.set("n", "<Leader>fn", ":lua MiniFiles.open()<CR>", { desc = "File Browser" })

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

			-- Notifications
			require("mini.notify").setup()

			-- Highlight word under cursor
			require("mini.cursorword").setup()

			-- Animations :D
			-- require("mini.animate").setup()

			-- MiniMap
			-- NOTE: Might introduce lag on very big buffers (10000+ lines)
			local map = require("mini.map")
			map.setup({
				-- Use Braille dots to encode text
				symbols = { encode = map.gen_encode_symbols.dot("4x2") },
				-- Show built-in search matches, 'mini.diff' hunks, and diagnostic entries
				integrations = {
					map.gen_integration.builtin_search(),
					map.gen_integration.diff(),
					map.gen_integration.diagnostic(),
				},
			})
			vim.keymap.set("n", "<Leader>`", "<Cmd>lua require('mini.map').toggle()<CR>", { desc = "Toggle MiniMap" })

			-- Map built-in navigation characters to force map refresh
			for _, key in ipairs({ "n", "N", "*", "#" }) do
				local rhs = key
					-- Also open enough folds when jumping to the next match
					.. "zv"
					.. "<Cmd>lua MiniMap.refresh({}, { lines = false, scrollbar = false })<CR>"
				vim.keymap.set("n", key, rhs)
			end

			-- Move any selection in any direction. Example usage in Normal mode:
			-- - `<M-j>`/`<M-k>` - move current line down / up
			-- - `<M-h>`/`<M-l>` - decrease / increase indent of current line
			--
			-- Example usage in Visual mode:
			-- - `<M-h>`/`<M-j>`/`<M-k>`/`<M-l>` - move selection left/down/up/right
			require("mini.move").setup()
		end,
	},
}
