-- Fix for lualine error that hasn't been fixed yet
-- WIP PR: https://github.com/LazyVim/LazyVim/pull/5900
return {
  "nvim-lualine/lualine.nvim",
  opts = function(_, opts)
    opts.sections.lualine_x[2] = LazyVim.lualine.status(LazyVim.config.icons.kinds.Copilot, function()
      local clients = package.loaded["copilot"] and LazyVim.lsp.get_clients({ name = "copilot", bufnr = 0 }) or {}
      if #clients > 0 then
        local status = require("copilot.status").data.status
        return (status == "InProgress" and "pending") or (status == "Warning" and "error") or "ok"
      end
    end)
  end,
}
