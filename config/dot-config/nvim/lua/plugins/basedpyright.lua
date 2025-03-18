-- Override type checking mode, and do full workspace diagnostics
return {
  "neovim/nvim-lspconfig",
  opts = {
    servers = {
      basedpyright = {
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
