return {
	-- Show key options when using shortcuts
	"folke/which-key.nvim",
	opts = {},
	config = function()
		local wk = require("which-key")

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
			{ "<leader>cA", require("tiny-code-action").code_action, desc = "Visualize Code Actions" },
			{ "<leader>cd", vim.lsp.buf.definition, desc = "Go To Definition" },
			{ "<leader>cD", vim.lsp.buf.declaration, desc = "Go to Declaration" },
			{ "<leader>ci", vim.lsp.buf.implementation, desc = "Go to Implementation" },
			{ "<leader>cf", require("conform").format, desc = "Format Buffer" },
			{ "<leader>cr", vim.lsp.buf.rename, desc = "Code Rename" },
			{ "<leader>ct", vim.lsp.buf.type_definition, desc = "Go To Type Definition" },

			{ "<leader>f", group = "file" },
			{ "<leader>fa", "<cmd>Telescope adjacent<CR>", desc = "Adjacent Files" },
			{ "<leader>fe", "<cmd>Explore<CR>", desc = "Explore" },
			{ "<leader>ff", "<cmd>Telescope find_files hidden=true<cr>", desc = "Find File" },
			{ "<leader>fr", "<cmd>Telescope oldfiles<cr>", desc = "Open Recent File" },

			{ "<leader>g", group = "git" },
			{ "<leader>gg", "<cmd>LazyGit<CR>", desc = "LazyGit" },
			{ "<leader>gb", "<cmd>Gitsigns toggle_current_line_blame<CR>", desc = "Current Line Blame" },

			{ "<leader>m", group = "markdown" },
			{ "<leader>mm", "<cmd>RenderMarkdown toggle<CR>", desc = "Toggle Markdown Render" },

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
			{ "<leader>ss", require("telescope.builtin").lsp_document_symbols, desc = "Document Symbols" },
			{ "<leader>st", "<cmd>TodoTelescope<CR>", desc = "Search TODO" },
			{ "<leader>su", "<cmd>Telescope undo<CR>", desc = "Search Undo" },
			{ "<leader>sc", require("telescope.builtin").grep_string, desc = "Search current Word" },

			{ "<leader>t", group = "toggle" },
			{ "<leader>tb", require("barbecue.ui").toggle, desc = "Barbecue (show code context winbar)" },
			{ "<leader>tn", "<cmd>Neotree toggle<CR>", desc = "NeoTree" },
			{ "<leader>ts", "<cmd>AerialToggle!<CR>", desc = "Symbols Tree" },
			{ "<leader>tt", "<cmd>ToggleTerm size=30<CR>", desc = "Terminal" },

			{ "<leader>w", group = "workspace" },
			{ "<leader>wd", require("telescope.builtin").diagnostics, desc = "Workspace Diagnostics" },
			{ "<leader>ws", require("telescope.builtin").lsp_dynamic_workspace_symbols, desc = "Workspace Symbols" },

			{ "<leader>x", group = "trouble" },
			{ "<leader>xl", "<cmd>Trouble loclist toggle<cr>", desc = "Location List (Trouble)" },
			{ "<leader>xq", "<cmd>Trouble qflist toggle<cr>", desc = "Quickfix List (Trouble)" },
			{ "<leader>xd", "<cmd>Trouble diagnostics toggle<cr>", desc = "Diagnostics (Trouble)" },
			{ "<leader>xb", "<cmd>Trouble diagnostics toggle filter.buf=0<cr>", desc = "Buffer Diagnostics (Trouble)" },
		})
	end,
}
