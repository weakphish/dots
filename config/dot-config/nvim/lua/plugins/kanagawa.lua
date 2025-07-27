return {
  "rebelot/kanagawa.nvim",
  config = function()
    require("kanagawa").setup({
      theme = "wave", -- Load "wave" theme
      background = { -- map the value of 'background' option to a theme
        dark = "wave", -- try "dragon" !
        light = "lotus",
      },
    })

    -- setup must be called before loading
    vim.cmd("colorscheme kanagawa")
  end,
}
