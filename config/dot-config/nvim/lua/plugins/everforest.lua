-- every spec file under the "plugins" directory will be loaded automatically by lazy.nvim
return {
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
        background = "hard",
      })
      require("everforest").load()
    end,
  },
}
