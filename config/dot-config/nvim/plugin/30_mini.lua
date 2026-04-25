-- MINI configuration

local now, now_if_args, later = Config.now, Config.now_if_args, Config.later

-- Step one ===================================================================

-- Colorscheme: gruvbox
now(function()
  vim.pack.add({ 'https://github.com/ellisonleao/gruvbox.nvim' })
  vim.o.background = 'dark'
  vim.cmd('colorscheme gruvbox')
end)

-- Common configuration presets
now(function()
  require('mini.basics').setup({
    options = { basic = false },
    mappings = {
      windows = true,
      move_with_alt = true,
    },
  })
end)

-- Icon provider
now(function()
  local ext3_blocklist = { scm = true, txt = true, yml = true }
  local ext4_blocklist = { json = true, yaml = true }
  require('mini.icons').setup({
    use_file_extension = function(ext, _)
      return not (ext3_blocklist[ext:sub(-3)] or ext4_blocklist[ext:sub(-4)])
    end,
  })

  later(MiniIcons.mock_nvim_web_devicons)
  later(MiniIcons.tweak_lsp_kind)
end)

-- Notifications
now(function() require('mini.notify').setup() end)

-- Session management
now(function() require('mini.sessions').setup() end)

-- Start screen
now(function() require('mini.starter').setup() end)

-- Statusline
now(function() require('mini.statusline').setup() end)

-- Tabline
now(function() require('mini.tabline').setup() end)

-- Step one or two ============================================================

-- Completion and signature help
now_if_args(function()
  local process_items_opts = { kind_priority = { Text = -1, Snippet = 99 } }
  local process_items = function(items, base)
    return MiniCompletion.default_process_items(items, base, process_items_opts)
  end
  require('mini.completion').setup({
    lsp_completion = {
      source_func = 'omnifunc',
      auto_setup = false,
      process_items = process_items,
    },
  })

  local on_attach = function(ev)
    vim.bo[ev.buf].omnifunc = 'v:lua.MiniCompletion.completefunc_lsp'
  end
  Config.new_autocmd('LspAttach', nil, on_attach, "Set 'omnifunc'")

  vim.lsp.config('*', { capabilities = MiniCompletion.get_lsp_capabilities() })
end)

-- Navigate and manipulate file system
now_if_args(function()
  require('mini.files').setup({ windows = { preview = true } })

  local add_marks = function()
    MiniFiles.set_bookmark('c', vim.fn.stdpath('config'), { desc = 'Config' })
    local vimpack_plugins = vim.fn.stdpath('data') .. '/site/pack/core/opt'
    MiniFiles.set_bookmark('p', vimpack_plugins, { desc = 'Plugins' })
    MiniFiles.set_bookmark('w', vim.fn.getcwd, { desc = 'Working directory' })
  end
  Config.new_autocmd('User', 'MiniFilesExplorerOpen', add_marks, 'Add bookmarks')
end)

-- Miscellaneous functions
now_if_args(function()
  require('mini.misc').setup()
  MiniMisc.setup_auto_root()
  MiniMisc.setup_restore_cursor()
  MiniMisc.setup_termbg_sync()
end)

-- Step two ===================================================================

-- Extra 'mini.nvim' functionality
later(function() require('mini.extra').setup() end)

-- Extend and create a/i textobjects
later(function()
  local ai = require('mini.ai')
  ai.setup({
    custom_textobjects = {
      B = MiniExtra.gen_ai_spec.buffer(),
      F = ai.gen_spec.treesitter({ a = '@function.outer', i = '@function.inner' }),
    },
    search_method = 'cover',
  })
end)

-- Align text interactively
later(function() require('mini.align').setup() end)

-- Go forward/backward with square brackets
later(function() require('mini.bracketed').setup() end)

-- Remove buffers
later(function() require('mini.bufremove').setup() end)

-- Show next key clues
later(function()
  local miniclue = require('mini.clue')
  -- stylua: ignore
  miniclue.setup({
    clues = {
      Config.leader_group_clues,
      miniclue.gen_clues.builtin_completion(),
      miniclue.gen_clues.g(),
      miniclue.gen_clues.marks(),
      miniclue.gen_clues.registers(),
      miniclue.gen_clues.square_brackets(),
      miniclue.gen_clues.windows({ submode_resize = true }),
      miniclue.gen_clues.z(),
    },
    triggers = {
      { mode = { 'n', 'x' }, keys = '<Leader>' },
      { mode =   'n',        keys = '\\' },
      { mode = { 'n', 'x' }, keys = '[' },
      { mode = { 'n', 'x' }, keys = ']' },
      { mode =   'i',        keys = '<C-x>' },
      { mode = { 'n', 'x' }, keys = 'g' },
      { mode = { 'n', 'x' }, keys = "'" },
      { mode = { 'n', 'x' }, keys = '`' },
      { mode = { 'n', 'x' }, keys = '"' },
      { mode = { 'i', 'c' }, keys = '<C-r>' },
      { mode =   'n',        keys = '<C-w>' },
      { mode = { 'n', 'x' }, keys = 's' },
      { mode = { 'n', 'x' }, keys = 'z' },
    },
  })
end)

-- Command line tweaks
later(function() require('mini.cmdline').setup() end)

-- Comment lines
later(function() require('mini.comment').setup() end)

-- Autohighlight word under cursor
later(function() require('mini.cursorword').setup() end)

-- Work with diff hunks
later(function()
  require('mini.diff').setup({
    view = {
      style = 'sign',
    },
  })
end)

-- Git integration
later(function() require('mini.git').setup() end)

-- Highlight patterns in text
later(function()
  local hipatterns = require('mini.hipatterns')
  local hi_words = MiniExtra.gen_highlighter.words
  hipatterns.setup({
    highlighters = {
      fixme = hi_words({ 'FIXME', 'Fixme', 'fixme' }, 'MiniHipatternsFixme'),
      hack = hi_words({ 'HACK', 'Hack', 'hack' }, 'MiniHipatternsHack'),
      todo = hi_words({ 'TODO', 'Todo', 'todo' }, 'MiniHipatternsTodo'),
      note = hi_words({ 'NOTE', 'Note', 'note' }, 'MiniHipatternsNote'),
      hex_color = hipatterns.gen_highlighter.hex_color(),
    },
  })
end)

-- Jump to next/previous single character
later(function() require('mini.jump').setup() end)

-- Special key mappings
later(function()
  require('mini.keymap').setup()
  MiniKeymap.map_multistep('i', '<Tab>', { 'pmenu_next' })
  MiniKeymap.map_multistep('i', '<S-Tab>', { 'pmenu_prev' })
  MiniKeymap.map_multistep('i', '<CR>', { 'pmenu_accept', 'minipairs_cr' })
  MiniKeymap.map_multistep('i', '<BS>', { 'minipairs_bs' })
end)

-- Window with text overview
later(function()
  local map = require('mini.map')
  map.setup({
    symbols = { encode = map.gen_encode_symbols.dot('4x2') },
    integrations = {
      map.gen_integration.builtin_search(),
      map.gen_integration.diff(),
      map.gen_integration.diagnostic(),
    },
  })

  for _, key in ipairs({ 'n', 'N', '*', '#' }) do
    local rhs = key
      .. 'zv'
      .. '<Cmd>lua MiniMap.refresh({}, { lines = false, scrollbar = false })<CR>'
    vim.keymap.set('n', key, rhs)
  end
end)

-- Move any selection in any direction
later(function() require('mini.move').setup() end)

-- Text edit operators
later(function()
  require('mini.operators').setup()

  vim.keymap.set('n', '(', 'gxiagxila', { remap = true, desc = 'Swap arg left' })
  vim.keymap.set('n', ')', 'gxiagxina', { remap = true, desc = 'Swap arg right' })
end)

-- Autopairs
later(function()
  require('mini.pairs').setup({ modes = { command = true } })
end)

-- Pick anything
later(function() require('mini.pick').setup() end)

-- Manage and expand snippets
later(function()
  local latex_patterns = { 'latex/**/*.json', '**/latex.json' }
  local lang_patterns = {
    tex = latex_patterns,
    plaintex = latex_patterns,
    markdown_inline = { 'markdown.json' },
  }

  local snippets = require('mini.snippets')
  local config_path = vim.fn.stdpath('config')
  snippets.setup({
    snippets = {
      snippets.gen_loader.from_file(config_path .. '/snippets/global.json'),
      snippets.gen_loader.from_lang({ lang_patterns = lang_patterns }),
    },
  })
end)

-- Split and join arguments
later(function() require('mini.splitjoin').setup() end)

-- Surround actions
later(function()
  require('mini.surround').setup({
    mappings = {
      add = '',
      delete = '',
      find = '',
      find_left = '',
      highlight = '',
      replace = '',
      suffix_last = 'l',
      suffix_next = 'n',
    },
  })
end)

-- Highlight and remove trailspace
later(function() require('mini.trailspace').setup() end)

-- Track and reuse file system visits
later(function() require('mini.visits').setup() end)
