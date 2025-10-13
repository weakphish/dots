-- Disable animations and adjust sizes
return {
  "folke/edgy.nvim",
  opts = {
    options = {
      left = { size = 50 },
      bottom = { size = 25 },
      right = { size = 50 },
      top = { size = 10 },
    },
    animate = {
      enabled = false,
    },
  },
}
