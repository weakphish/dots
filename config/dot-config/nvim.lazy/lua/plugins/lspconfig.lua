return {
  "neovim/nvim-lspconfig",
  opts = {
    inlay_hints = { enabled = false }, -- Disable inlay hints by default
    servers = {
      basedpyright = { -- Override type checking mode, and do full workspace diagnostics
        settings = {
          basedpyright = {
            analysis = {
              typeCheckingMode = "standard",
              diagnosticMode = "workspace",
            },
          },
        },
      },
    },
  },
}
