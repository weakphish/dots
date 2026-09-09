-- Language servers, formatter installation, formatting, and snippets.

local add = vim.pack.add
local now_if_args, later = Config.now_if_args, Config.later
local now = Config.now

-- Mason (LSP/formatter installer) ============================================
now_if_args(function()
  add({ "https://github.com/mason-org/mason.nvim" })
  require("mason").setup()
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
vim.filetype.add({ pattern = { [".*%.ya?ml%.gotmpl"] = "helm" } })
vim.filetype.add({ extension = { tpl = "helm" } })
later(function()
  add({ "https://github.com/qvalentin/helm-ls.nvim" })
  vim.lsp.config("helm_ls", { root_markers = { "Chart.yaml", ".git" } })
  require("helm-ls").setup()
end)

-- Go plugin
now(function()
    add({"https://github.com/ray-x/go.nvim"})
    require('go').setup()
end)

-- Language servers ============================================================
now_if_args(function()
  -- nvim-lspconfig itself is NOT deprecated. It provides server-specific configs.
  -- The configs live in the lsp/ directory. vim.lsp.config automatically finds them and merges them with any local lsp/*.lua configs defined by you or a plugin.
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
