-- Override edgy integration
return {
  "folke/edgy.nvim",
  optional = true,
  opts = function(_, opts)
    opts.right = opts.right or {}
    for i, panel in ipairs(opts.right) do
      if panel.ft == "copilot-chat" then
        table.remove(opts.right, i)
        break
      end
    end
    table.insert(opts.right, {
      ft = "copilot-chat",
      title = "Copilot Chat",
      size = { width = 0.5 },
    })
  end,
}
