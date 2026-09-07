-- Completion and signature help.

local add = vim.pack.add
local now_if_args = Config.now_if_args

now_if_args(function()
  add({
    { src = "https://github.com/Saghen/blink.cmp", version = vim.version.range("1") },
  })

  local cmp = require("blink.cmp")
  cmp.setup({
    keymap = {
      preset = "enter",
      ["<Tab>"] = { "select_next", "snippet_forward", "fallback" },
      ["<S-Tab>"] = { "select_prev", "snippet_backward", "fallback" },
    },
    completion = {
      documentation = { auto_show = true },
      menu = {
        draw = {
          columns = { { "label", "label_description", gap = 1 }, { "kind_icon", "kind", gap = 1 } } }
      },
    },
    snippets = {
      preset = "mini_snippets",
    },
    sources = {
      default = { "lsp", "path", "snippets", "buffer" },
    },
    fuzzy = {
      implementation = "prefer_rust_with_warning"
    },
    signature = { enabled = true },
  })

  vim.lsp.config("*", { capabilities = cmp.get_lsp_capabilities() })
end)
