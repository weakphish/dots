return {
  -- Linter for Jenkinsfiles
  {
    'ckipp01/nvim-jenkinsfile-linter',
    dependencies = { 'nvim-lua/plenary.nvim' },
  },

  {
    -- Zen mode for writing markdown
    'folke/zen-mode.nvim',
    config = function()
      require('zen-mode').setup {}
    end,
  },

  {
    -- Dim inactive portions of code
    'folke/twilight.nvim',
    config = function()
      require('twilight').setup {}
    end,
  },

  {
    -- Use wiki links in markdown
    'jakewvincent/mkdnflow.nvim',
    config = function()
      require('mkdnflow').setup()
    end,
  },
}
