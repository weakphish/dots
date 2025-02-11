return {
  -- change trouble config
  {
    "folke/trouble.nvim",
    -- opts will be merged with the parent spec
    opts = {
      modes = {
        symbols = {
          win = { position = "right", size = 50 },
        },
      },
    },
  },
}
