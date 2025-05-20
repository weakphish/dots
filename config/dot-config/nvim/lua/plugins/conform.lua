return {
  "stevearc/conform.nvim",
  opts = function()
    ---@type conform.setupOpts
    local opts = {
      formatters_by_ft = {
        ["python"] = { "black", "isort" },
      },
    }
    return opts
  end,
}
