return {
	-- Show key options when using shortcuts
	"folke/which-key.nvim",
	opts = {},
	config = function()
		local wk = require("which-key")
		local dap = require("dap")

		-- Normal mode
		wk.add({
			{ "<leader>b", group = "buffer" },
			{ "<leader>bb", require("telescope.builtin").buffers, desc = "Find Buffer" },
			{ "<leader>bd", "<cmd>bd<CR>", desc = "Delete Buffer" },
			{ "<leader>bn", "<cmd>bn<CR>", desc = "Next Buffer" },
			{ "<leader>bp", "<cmd>bp<CR>", desc = "Prev Buffer" },
			{ "<leader>bo", "<cmd>BufferLinePick<CR>", desc = "Pick Buffer From Line" },

			{ "<leader>c", group = "code" },
			{ "<leader>ca", vim.lsp.buf.code_action, desc = "Code Action" },
			{ "<leader>cd", vim.lsp.buf.definition, desc = "Go To Definition" },
			{ "<leader>cD", vim.lsp.buf.declaration, desc = "Go to Declaration" },
			{ "<leader>ci", vim.lsp.buf.implementation, desc = "Go to Implementation" },
			{ "<leader>cf", require("conform").format, desc = "Format Buffer" },
			{ "<leader>cr", vim.lsp.buf.rename, desc = "Code Rename" },
			{ "<leader>ct", vim.lsp.buf.type_definition, desc = "Go To Type Definition" },

			{ "<leader>d", group = "d" },
			{ "<leader>db", dap.toggle_breakpoint, desc = "Toggle breakpoint" },
			{
				"<leader>cB",
				function()
					dap.set_breakpoint(vim.fn.input("[B]reakpoint condition: "))
				end,
				desc = "Breakpoint condition",
			},
			{ "<leader>dc", dap.continue, desc = "Continue" },
			{ "<leader>di", dap.step_into, desc = "Step into" },
			{ "<leader>do", dap.step_over, desc = "Step over" },
			{ "<leader>du", dap.step_out, desc = "Step up (out)" },

			{ "<leader>f", group = "file" },
			{ "<leader>fa", "<cmd>Telescope adjacent<CR>", desc = "Adjacent Files" },
			{ "<leader>ff", "<cmd>Telescope find_files hidden=true<cr>", desc = "Find File" },
			{ "<leader>fr", "<cmd>Telescope oldfiles<cr>", desc = "Open Recent File" },

			{ "<leader>g", group = "git" },
			{ "<leader>gg", "<cmd>LazyGit<CR>", desc = "LazyGit" },
			{ "<leader>gb", "<cmd>Gitsigns toggle_current_line_blame<CR>", desc = "Current Line Blame" },

			{ "<leader>m", group = "markdown" },
			{ "<leader>mm", "<cmd>MarkdownPreview<CR>", desc = "Open Preview" },
			{ "<leader>mc", "<cmd>MarkdownPreviewStop<CR>", desc = "Close Preview" },
			{ "<leader>mf", "<cmd>ZenMode<CR>", desc = "Focus" },
			{ "<leader>mt", "<cmd>Twilight<CR>", desc = "Toggle Dim Inactive Code" },

			{ "<leader>n", group = "(Neo)test" },
			{ "<leader>nr", "<cmd>Neotest run file<CR>", desc = "Run File" },
			{ "<leader>ns", "<cmd>Neotest summary<CR>", desc = "Summary" },
			{ "<leader>no", "<cmd>Neotest output<CR>", desc = "Output" },

			{ "<leader>s", group = "search" },
			{
				"<leader>sb",
				require("telescope.builtin").current_buffer_fuzzy_find,
				desc = "Fuzzily search current buffer",
			},
			{ "<leader>sd", require("telescope.builtin").diagnostics, desc = "Search Diagnostics" },
			{ "<leader>sg", require("telescope.builtin").live_grep, desc = "Search with Grep" },
			{ "<leader>sh", require("telescope.builtin").help_tags, desc = "Search Help" },
			{ "<leader>sj", require("telescope.builtin").jumplist, desc = "Search Jumplist" },
			{ "<leader>sr", require("telescope.builtin").lsp_references, desc = "Search References" },
			{ "<leader>sw", require("telescope.builtin").lsp_document_symbols, desc = "Workspace Document Symbols" },
			{ "<leader>st", "<cmd>TodoTelescope<CR>", desc = "Search TODO" },
			{ "<leader>su", "<cmd>Telescope undo<CR>", desc = "Search Undo" },
			{ "<leader>sc", require("telescope.builtin").grep_string, desc = "Search current Word" },

			{ "<leader>t", group = "toggle" },
			{ "<leader>tb", require("barbecue.ui").toggle, desc = "Barbecue (show code context winbar)" },
			{ "<leader>tn", "<cmd>Neotree toggle<CR>", desc = "NeoTree" },
			{ "<leader>ts", "<cmd>AerialToggle!<CR>", desc = "Symbols Tree" },
			{ "<leader>tt", "<cmd>:ToggleTerm size=30<CR>", desc = "Toggle Terminal" },
			{ "<leader>tf", "<cmd>:ToggleTerm direction=float<CR>", desc = "Toggle Floating Terminal" },

			{ "<leader>w", group = "Workspace" },
			{ "<leader>wd", require("telescope.builtin").diagnostics, desc = "Workspace Diagnostics" },
			{ "<leader>ws", require("telescope.builtin").lsp_dynamic_workspace_symbols, desc = "Workspace Symbols" },

			{ "<leader>x", group = "Trouble" },
			{ "<leader>xi", "<cmd>Trouble lsp_incoming_calls<cr>", desc = "Incoming Calls (Trouble)" },
			{ "<leader>xl", "<cmd>Trouble loclist toggle<cr>", desc = "Location List (Trouble)" },
			{ "<leader>xq", "<cmd>Trouble qflist toggle<cr>", desc = "Quickfix List (Trouble)" },
			{ "<leader>xr", "<cmd>Trouble lsp_references<cr>", desc = "References (Trouble)" },
			{ "<leader>xd", "<cmd>Trouble diagnostics toggle<cr>", desc = "Diagnostics (Trouble)" },
			{ "<leader>xb", "<cmd>Trouble diagnostics toggle filter.buf=0<cr>", desc = "Buffer Diagnostics (Trouble)" },

			{
				{ "X", "<Plug>(leap-backward-till)", desc = "Leap Backward Till", mode = "v" },
				{ "x", "<Plug>(leap-forward-till)", desc = "Leap Forward Till", mode = "v" },
			},
		})
	end,
}
