-- Keymaps are automatically loaded on the VeryLazy event
-- Default keymaps that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/keymaps.lua
-- Add any additional keymaps here
vim.keymap.set("n", "<leader>bo", "<cmd>BufferLinePick<CR>", { desc = "Open Buffer", remap = true })

-- MyPy/Quickfix integration
local function run_mypy_with_errorformat()
  local current_file = vim.api.nvim_buf_get_name(0)
  vim.cmd("compiler mypy") -- Ensure the 'mypy' compiler is set (see below)
  vim.cmd("make " .. vim.fn.shellescape(current_file))
  vim.cmd("copen")
end

vim.api.nvim_create_user_command("RunMyPyEF", run_mypy_with_errorformat, { desc = "Run MyPy with errorformat" })
vim.keymap.set("n", "<leader>cp", ":RunMyPyEF<CR>", { desc = "Run MyPy with errorformat" })

-- Obsidian
vim.keymap.set("n", "<leader>od", ":ObsidianToday<CR>", { desc = "Today's Daily Note" })
vim.keymap.set("n", "<leader>ot", ":ObsidianTags<CR>", { desc = "Tags" })
vim.keymap.set("n", "<leader>oy", ":ObsidianYesterday<CR>", { desc = "Yesterday's Daily Note" })
