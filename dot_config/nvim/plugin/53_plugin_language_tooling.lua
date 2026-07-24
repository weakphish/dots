-- Language servers, formatter installation, formatting, and snippets.

local add = vim.pack.add
local now_if_args, later = Config.now_if_args, Config.later
local now = Config.now

-- Mason (LSP/formatter installer) ============================================
now_if_args(function()
  add({ "https://github.com/mason-org/mason.nvim" })
  require("mason").setup()
end)

-- Formatting =================================================================
later(function()
  add({ "https://github.com/stevearc/conform.nvim" })

  require("conform").setup({
    default_format_opts = {
      lsp_format = "fallback",
    },
    formatters_by_ft = {
      lua = { "stylua" },
      python = { "isort", "black" },
      rust = { "rustfmt", lsp_format = "fallback" },
      json = { "prettierd", "prettier", stop_after_first = true },
      jsonc = { "prettierd", "prettier", stop_after_first = true },
      javascript = { "prettierd", "prettier", stop_after_first = true },
      typescript = { "prettierd", "prettier", stop_after_first = true },
      yaml = { "prettierd", "prettier", stop_after_first = true },
      go = { "gofmt" },
      markdown = { "prettierd", "prettier", stop_after_first = true },
    },
    format_on_save = {
      timeout_ms = 500,
      lsp_format = "fallback",
    },
  })
end)

-- Snippets ===================================================================
later(function()
  add({ "https://github.com/rafamadriz/friendly-snippets" })
end)

-- Markdown! ==================================================================
later(function()
  add({ "https://github.com/MeanderingProgrammer/render-markdown.nvim" })
end)

-- Helm Plugin
later(function()
  add({ "https://github.com/qvalentin/helm-ls.nvim" })
  vim.filetype.add({ pattern = { [".*%.ya?ml%.gotmpl"] = "helm" } })
  vim.lsp.config("helm_ls", { root_markers = { "Chart.yaml", ".git" } })
  require("helm-ls").setup()
end)

-- Language servers ============================================================
now_if_args(function()
  add({
    "https://github.com/neovim/nvim-lspconfig",
  })

  vim.lsp.enable({
    "ty",
    "ruff",
    "ts_ls",
    "gopls",
    "bashls",
    "rust_analyzer",
    "gh_actions_ls",
    "yamlls",
    "helm_ls",
    "lua_ls",
    "marksman",
    "jdtls",
  })
end)
