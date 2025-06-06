-- Autocmds are automatically loaded on the VeryLazy event
-- Default autocmds that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/autocmds.lua
--
-- Add any additional autocmds here
-- with `vim.api.nvim_create_autocmd`
--
-- Or remove existing autocmds by their group name (which is prefixed with `lazyvim_` for the defaults)
-- e.g. vim.api.nvim_del_augroup_by_name("lazyvim_wrap_spell")

-- Godot prefers 4 spaces > tabs
vim.api.nvim_create_autocmd("FileType", {
  pattern = { "gdscript" },
  callback = function()
    vim.bo.sw = 4
    vim.bo.sts = 4
    vim.bo.ts = 4
    vim.bo.expandtab = false
    vim.bo.softtabstop = 4
  end,
})

-- Disable autoformat for YAML and Markdown files
vim.api.nvim_create_autocmd({ "FileType" }, {
  pattern = { "md", "yml", "yaml" },
  callback = function()
    vim.b.autoformat = false
  end,
})
