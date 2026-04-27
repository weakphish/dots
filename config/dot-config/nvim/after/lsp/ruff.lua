-- Disable using Ruff for formatting because I use Black/isort in almost all cases
return {
  on_attach = function(client)
    client.server_capabilities.documentFormattingProvider = false
    client.server_capabilities.documentRangeFormattingProvider = false
  end,
}
