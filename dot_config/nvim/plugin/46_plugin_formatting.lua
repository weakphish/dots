local add = vim.pack.add
local now_if_args, later = Config.now_if_args, Config.later
local now = Config.now

-- Formatting =================================================================
now(function()
  add({ "https://github.com/stevearc/conform.nvim" })

  require("conform").setup({
    default_format_opts = {
      lsp_format = "fallback",
    },
    formatters_by_ft = {
      lua = { "stylua" },
      python = { "isort", "black" },
      rust = { "rustfmt", lsp_format = "fallback" },
      json = { "prettier", stop_after_first = true },
      jsonc = { "prettier", stop_after_first = true },
      javascript = { "prettier", stop_after_first = true },
      typescript = { "prettier", stop_after_first = true },
      yaml = { "prettier", stop_after_first = true },
      go = { "gofmt" },
      markdown = { "prettier", stop_after_first = true },
    },
    format_on_save = {
      timeout_ms = 500,
      lsp_format = "fallback",
    },
  })
  require("conform").formatters.prettier = {
    prepend_args = {
      "--tab-width", "4",
      -- CRITICAL: Tells prettier to prioritize CLI args over local config files
      -- Use "prefer-file" if you want local project files to override this global '4'
      "--config-precedence", "file-override" }
  }
end)
