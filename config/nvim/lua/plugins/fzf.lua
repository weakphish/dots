return {
	"ibhagwan/fzf-lua",
	-- optional for icon support
	-- dependencies = { "nvim-tree/nvim-web-devicons" },
	-- or if using mini.icons/mini.nvim
	dependencies = { "nvim-mini/mini.icons" },
	opts = {},
	keys = {
		{ "<leader>bb", "<cmd>FzfLua buffers<cr>", desc = "Buffers" },
		{ "<leader>ff", "<cmd>FzfLua files<cr>", desc = "Files" },
		{ "<leader>sg", "<cmd>FzfLua live_grep_native<cr>", desc = "Grep" },
		{ "<leader>sv", "<cmd>FzfLua grep_visual<cr>", desc = "Grep Visual Selection", mode = "v" },
		{ "<leader>sr", "<cmd>FzfLua lsp_references<cr>", desc = "References" },
		{ "<leader>sd", "<cmd>FzfLua lsp_definitions<cr>", desc = "Definitions" },
		{ "<leader>sy", "<cmd>FzfLua lsp_typedefs<cr>", desc = "Type Definitions" },
		{ "<leader>ss", "<cmd>FzfLua lsp_document_symbols<cr>", desc = "Document Symbols" },
		{ "<leader>sS", "<cmd>FzfLua lsp_workspace_symbols<cr>", desc = "Workspace Symbols" },
		{ "<leader>si", "<cmd>FzfLua lsp_incoming_calls<cr>", desc = "Incoming Calls" },
		{ "<leader>so", "<cmd>FzfLua lsp_outgoing_calls<cr>", desc = "Outgoing Calls" },
		{ "<leader>sj", "<cmd>FzfLua jumps<cr>", desc = "Jumplist" },
	},
}
