return {
  cmd = { 'gopls' },
  filetypes = { 'go', 'gomod', 'gowork', 'gotmpl' },
  settings = {
    gopls = {
      codelenses = {
        generate = true, -- show gc_details
        regenerate_cgo = true,
        test = true,     -- show "run test" lenses
        tidy = true,
      },
    },
  },
}
