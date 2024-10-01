return {
	-- Linter for Jenkinsfiles
	{
		"ckipp01/nvim-jenkinsfile-linter",
		dependencies = { "nvim-lua/plenary.nvim" },
	},

	{
		-- Use wiki links in markdown
		"jakewvincent/mkdnflow.nvim",
		config = function()
			require("mkdnflow").setup({
				modules = {
					folds = false,
					foldtext = false,
				},
			})
		end,
	},
}
