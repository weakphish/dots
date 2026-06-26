-- Custom mappings

-- General mappings ===========================================================
local nmap = function(lhs, rhs, desc)
	vim.keymap.set("n", lhs, rhs, { desc = desc })
end

nmap("[p", '<Cmd>exe "iput! " . v:register<CR>', "Paste Above")
nmap("]p", '<Cmd>exe "iput "  . v:register<CR>', "Paste Below")
vim.keymap.set({ "n", "x", "o" }, "s", function()
	require("flash").jump()
end, { desc = "Flash" })
vim.keymap.set({ "n", "x", "o" }, "S", function()
	require("flash").treesitter()
end, { desc = "Flash Treesitter" })
vim.keymap.set("o", "r", function()
	require("flash").remote()
end, { desc = "Remote Flash" })
vim.keymap.set({ "o", "x" }, "R", function()
	require("flash").treesitter_search()
end, { desc = "Treesitter Search" })
vim.keymap.set("c", "<C-s>", function()
	require("flash").toggle()
end, { desc = "Toggle Flash Search" })
vim.keymap.set("n", "<Esc><Esc>", "<Cmd>nohlsearch<CR>", { silent = true, desc = "Clear search highlight" })

-- stylua: ignore start

-- Leader mappings ============================================================
Config.leader_group_clues = {
  { mode = 'n', keys = '<Leader>b', desc = '+Buffer' },
  { mode = 'n', keys = '<Leader>e', desc = '+Explore/Edit' },
  { mode = 'n', keys = '<Leader>f', desc = '+Find' },
  { mode = 'n', keys = '<Leader>g', desc = '+Git' },
  { mode = 'n', keys = '<Leader>l', desc = '+Language' },
  { mode = 'n', keys = '<Leader>m', desc = '+Map' },
  { mode = 'n', keys = '<Leader>n', desc = '+Notes' },
  { mode = 'n', keys = '<Leader>o', desc = '+Other' },
  { mode = 'n', keys = '<Leader>p', desc = '+Plugin' },
  { mode = 'n', keys = '<Leader>S', desc = '+Session' },
  { mode = 'n', keys = '<Leader>s', desc = '+Surround' },
  { mode = 'x', keys = '<Leader>s', desc = '+Surround' },
  { mode = 'n', keys = '<Leader>t', desc = '+Terminal' },
  { mode = 'n', keys = '<Leader>v', desc = '+Visits' },
  { mode = 'n', keys = '<Leader>x', desc = '+Trouble' },

  { mode = 'x', keys = '<Leader>g', desc = '+Git' },
  { mode = 'x', keys = '<Leader>l', desc = '+Language' },
}

local nmap_leader = function(suffix, rhs, desc)
  vim.keymap.set('n', '<Leader>' .. suffix, rhs, { desc = desc })
end
local xmap_leader = function(suffix, rhs, desc)
  vim.keymap.set('x', '<Leader>' .. suffix, rhs, { desc = desc })
end

-- b is for 'Buffer'
local new_scratch_buffer = function()
  vim.api.nvim_win_set_buf(0, vim.api.nvim_create_buf(true, true))
end

nmap_leader('ba', '<Cmd>b#<CR>',                                 'Alternate')
nmap_leader('bd', '<Cmd>lua MiniBufremove.delete()<CR>',         'Delete')
nmap_leader('bD', '<Cmd>lua MiniBufremove.delete(0, true)<CR>',  'Delete!')
nmap_leader('bs', new_scratch_buffer,                            'Scratch')
nmap_leader('bw', '<Cmd>lua MiniBufremove.wipeout()<CR>',        'Wipeout')
nmap_leader('bW', '<Cmd>lua MiniBufremove.wipeout(0, true)<CR>', 'Wipeout!')
nmap_leader('bn', '<Cmd>bnext<CR>', 'Next Buffer')
nmap_leader('bb', '<Cmd>bprevious<CR>', 'Previous Buffer')
nmap_leader('bo', '<Cmd>BufferLinePick<CR>', 'Pick buffer' )

-- e is for 'Explore' and 'Edit'
local edit_plugin_file = function(filename)
  return string.format('<Cmd>edit %s/plugin/%s<CR>', vim.fn.stdpath('config'), filename)
end
local explore_at_file = '<Cmd>Neotree reveal<CR>'
local explore_quickfix = function()
  vim.cmd(vim.fn.getqflist({ winid = true }).winid ~= 0 and 'cclose' or 'copen')
end
local explore_locations = function()
  vim.cmd(vim.fn.getloclist(0, { winid = true }).winid ~= 0 and 'lclose' or 'lopen')
end

nmap_leader('ed', '<Cmd>Neotree toggle<CR>',                'Directory')
nmap_leader('ef', explore_at_file,                          'File directory')
nmap_leader('ei', '<Cmd>edit $MYVIMRC<CR>',                 'init.lua')
nmap_leader('ek', edit_plugin_file('20_keymaps.lua'),       'Keymaps config')
nmap_leader('em', edit_plugin_file('30_mini.lua'),          'MINI config')
nmap_leader('en', '<Cmd>lua MiniNotify.show_history()<CR>', 'Notifications')
nmap_leader('eo', edit_plugin_file('10_options.lua'),       'Options config')
nmap_leader('ep', edit_plugin_file('40_plugins.lua'),       'Plugins config')
nmap_leader('eq', explore_quickfix,                         'Quickfix list')
nmap_leader('eQ', explore_locations,                        'Location list')

-- f is for 'Find'
--- Opens a fzf-lua picker for staged git hunks.
---
--- fzf-lua's hunk picker defaults to unstaged changes. This wrapper keeps the
--- old "Added hunks" mappings pointed at the index by using `git diff --cached`.
---
--- @param file string|nil Optional path to limit the staged hunk search.
--- @return nil
local find_staged_hunks = function(file)
  require('fzf-lua').git_hunks({
    cmd = 'git --no-pager diff --cached --color=always {file}',
    file = file,
  })
end

--- Opens a fzf-lua picker for staged hunks in the current buffer.
---
--- @return nil
local find_staged_hunks_buf = function()
  find_staged_hunks(vim.fn.expand('%'))
end

--- Opens a fzf-lua picker for unstaged hunks in the current buffer.
---
--- @return nil
local find_modified_hunks_buf = function()
  require('fzf-lua').git_hunks({ file = vim.fn.expand('%') })
end

--- Opens a fzf-lua picker for MiniVisits paths.
---
--- MiniVisits owns the visit index and sorting rules. Calling its select API
--- keeps those semantics while fzf-lua handles `vim.ui.select`.
---
--- @param cwd string|nil Visit scope; `""` means all tracked directories.
--- @param opts table|nil Options forwarded to `MiniVisits.list_paths()`.
--- @return nil
local find_visit_paths = function(cwd, opts)
  MiniVisits.select_path(cwd, opts)
end

nmap_leader('f/', '<Cmd>FzfLua search_history<CR>',              '"/" history')
nmap_leader('f:', '<Cmd>FzfLua command_history<CR>',             '":" history')
nmap_leader('fa', function() find_staged_hunks() end,            'Added hunks (all)')
nmap_leader('fA', find_staged_hunks_buf,                         'Added hunks (buf)')
nmap_leader('fb', '<Cmd>FzfLua buffers<CR>',                     'Buffers')
nmap_leader('fc', '<Cmd>FzfLua git_commits<CR>',                 'Commits (all)')
nmap_leader('fC', '<Cmd>FzfLua git_bcommits<CR>',                'Commits (buf)')
nmap_leader('fd', '<Cmd>FzfLua diagnostics_workspace<CR>',       'Diagnostic workspace')
nmap_leader('fD', '<Cmd>FzfLua diagnostics_document<CR>',        'Diagnostic buffer')
nmap_leader('ff', '<Cmd>FzfLua files<CR>',                       'Files')
nmap_leader('fg', '<Cmd>FzfLua live_grep<CR>',                   'Grep live')
nmap_leader('fG', '<Cmd>FzfLua grep_cword<CR>',                  'Grep current word')
nmap_leader('fh', '<Cmd>FzfLua helptags<CR>',                    'Help tags')
nmap_leader('fH', '<Cmd>FzfLua highlights<CR>',                  'Highlight groups')
nmap_leader('fj', '<Cmd>FzfLua jumps<CR>',                       'Jumplist')
nmap_leader('fl', '<Cmd>FzfLua lines<CR>',                       'Lines (all)')
nmap_leader('fL', '<Cmd>FzfLua blines<CR>',                      'Lines (buf)')
nmap_leader('fm', '<Cmd>FzfLua git_hunks<CR>',                   'Modified hunks (all)')
nmap_leader('fM', find_modified_hunks_buf,                       'Modified hunks (buf)')
nmap_leader('fr', '<Cmd>FzfLua resume<CR>',                      'Resume')
nmap_leader('fR', '<Cmd>FzfLua lsp_references<CR>',              'References (LSP)')
nmap_leader('fs', '<Cmd>FzfLua lsp_live_workspace_symbols<CR>',  'Symbols workspace (live)')
nmap_leader('fS', '<Cmd>FzfLua lsp_document_symbols<CR>',        'Symbols document')
nmap_leader('fv', function() find_visit_paths('') end,           'Visit paths (all)')
nmap_leader('fV', function() find_visit_paths() end,             'Visit paths (cwd)')

-- g is for 'Git'
local git_log_cmd = [[Git log --pretty=format:\%h\ \%as\ │\ \%s --topo-order]]
local git_log_buf_cmd = git_log_cmd .. ' --follow -- %'

nmap_leader('ga', '<Cmd>Git diff --cached<CR>',             'Added diff')
nmap_leader('gA', '<Cmd>Git diff --cached -- %<CR>',        'Added diff buffer')
nmap_leader('gc', '<Cmd>Git commit<CR>',                    'Commit')
nmap_leader('gC', '<Cmd>Git commit --amend<CR>',            'Commit amend')
nmap_leader('gd', '<Cmd>Git diff<CR>',                      'Diff')
nmap_leader('gD', '<Cmd>Git diff -- %<CR>',                 'Diff buffer')
nmap_leader('gg', '<Cmd>LazyGit<CR>',                       'LazyGit')
nmap_leader('gl', '<Cmd>' .. git_log_cmd .. '<CR>',         'Log')
nmap_leader('gL', '<Cmd>' .. git_log_buf_cmd .. '<CR>',     'Log buffer')
nmap_leader('go', '<Cmd>lua MiniDiff.toggle_overlay()<CR>', 'Toggle overlay')
nmap_leader('gs', '<Cmd>lua MiniGit.show_at_cursor()<CR>',  'Show at cursor')

xmap_leader('gs', '<Cmd>lua MiniGit.show_at_cursor()<CR>', 'Show at selection')

-- l is for 'Language'
nmap_leader('la', '<Cmd>lua vim.lsp.buf.code_action()<CR>',                       'Actions')
nmap_leader('lD', '<Cmd>Trouble diagnostics toggle filter.buf=0<CR>',              'Diagnostics list')
nmap_leader('ld', '<Cmd>lua vim.diagnostic.open_float()<CR>',                     'Diagnostic popup')
nmap_leader('lf', '<Cmd>lua require("conform").format()<CR>',                     'Format')
nmap_leader('li', '<Cmd>lua vim.lsp.buf.implementation()<CR>',                    'Implementation')
nmap_leader('lh', '<Cmd>lua vim.lsp.buf.hover()<CR>',                             'Hover')
nmap_leader('ll', '<Cmd>lua vim.lsp.codelens.run()<CR>',                          'Lens')
nmap_leader('lr', '<Cmd>lua vim.lsp.buf.rename()<CR>',                            'Rename')
nmap_leader('lR', '<Cmd>lua vim.lsp.buf.references()<CR>',                        'References')
nmap_leader('lS', '<Cmd>Trouble symbols toggle focus=false<CR>',                  'Symbols list')
nmap_leader('ls', '<Cmd>lua vim.lsp.buf.definition()<CR>',                        'Source definition')
nmap_leader('lt', '<Cmd>lua vim.lsp.buf.type_definition()<CR>',                   'Type definition')
nmap_leader('lX', '<Cmd>Trouble lsp toggle focus=false win.position=right<CR>',   'LSP list')

xmap_leader('lf', '<Cmd>lua require("conform").format()<CR>', 'Format selection')

-- m is for 'Map'
nmap_leader('mf', '<Cmd>lua MiniMap.toggle_focus()<CR>', 'Focus (toggle)')
nmap_leader('mr', '<Cmd>lua MiniMap.refresh()<CR>',      'Refresh')
nmap_leader('ms', '<Cmd>lua MiniMap.toggle_side()<CR>',  'Side (toggle)')
nmap_leader('mt', '<Cmd>lua MiniMap.toggle()<CR>',       'Toggle')

-- n is for 'Notes'
nmap_leader('nb', '<Cmd>Obsidian backlinks<CR>',          'Backlinks')
nmap_leader('nc', '<Cmd>Obsidian toc<CR>',                'Contents')
nmap_leader('nd', '<Cmd>Obsidian dailies<CR>',            'Daily notes')
nmap_leader('nf', '<Cmd>Obsidian new_from_template<CR>',  'New from template')
nmap_leader('nl', '<Cmd>Obsidian links<CR>',              'Links')
nmap_leader('nn', '<Cmd>Obsidian new<CR>',                'New note')
nmap_leader('no', '<Cmd>Obsidian open<CR>',               'Open in Obsidian')
nmap_leader('np', '<Cmd>Obsidian paste_img<CR>',          'Paste image')
nmap_leader('nq', '<Cmd>Obsidian quick_switch<CR>',       'Quick switch')
nmap_leader('nr', '<Cmd>Obsidian rename<CR>',             'Rename note')
nmap_leader('ns', '<Cmd>Obsidian search<CR>',             'Search')
nmap_leader('nt', '<Cmd>Obsidian today<CR>',              'Today')
nmap_leader('nT', '<Cmd>Obsidian tomorrow<CR>',           'Tomorrow')
nmap_leader('nu', '<Cmd>Obsidian unique_note<CR>',        'Unique note')
nmap_leader('nw', '<Cmd>Obsidian workspace<CR>',          'Workspace')
nmap_leader('nx', '<Cmd>Obsidian toggle_checkbox<CR>',    'Toggle checkbox')
nmap_leader('ny', '<Cmd>Obsidian yesterday<CR>',          'Yesterday')
nmap_leader('nz', '<Cmd>Obsidian tags<CR>',               'Tags')

xmap_leader('ne', '<Cmd>Obsidian extract_note<CR>', 'Extract note')
xmap_leader('nl', '<Cmd>Obsidian link<CR>',         'Link selection')
xmap_leader('nn', '<Cmd>Obsidian link_new<CR>',     'New linked note')

-- o is for 'Other'
nmap_leader('oh', '<Cmd>nohlsearch<CR>',                                                                          'Clear search highlight')
nmap_leader('or', '<Cmd>lua MiniMisc.resize_window()<CR>',                                                      'Resize to default width')
nmap_leader('os', '<Cmd>lua require("grug-far").open({ prefills = { search = vim.fn.expand("<cword>") } })<CR>', 'Search and replace')
nmap_leader('ot', '<Cmd>lua MiniTrailspace.trim()<CR>',                                                          'Trim trailspace')
nmap_leader('oz', '<Cmd>lua MiniMisc.zoom()<CR>',                                                                'Zoom toggle')

-- p is for 'Plugin'
local update_plugins = function()
  vim.pack.update()
end

nmap_leader('pu', update_plugins, 'Update')

-- s is for 'Session'
local session_new = 'MiniSessions.write(vim.fn.input("Session name: "))'

nmap_leader('Sd', '<Cmd>lua MiniSessions.select("delete")<CR>', 'Delete')
nmap_leader('Sn', '<Cmd>lua ' .. session_new .. '<CR>',         'New')
nmap_leader('Sr', '<Cmd>lua MiniSessions.select("read")<CR>',   'Read')
nmap_leader('Sw', '<Cmd>lua MiniSessions.write()<CR>',          'Write current')

-- t is for 'Terminal'
nmap_leader('tT', '<Cmd>horizontal term<CR>', 'Terminal (horizontal)')
nmap_leader('tt', '<Cmd>vertical term<CR>',   'Terminal (vertical)')

-- v is for 'Visits'
local make_pick_core = function(cwd)
  return function()
    local sort_latest = MiniVisits.gen_sort.default({ recency_weight = 1 })
    find_visit_paths(cwd, { filter = 'core', sort = sort_latest })
  end
end

nmap_leader('vc', make_pick_core(''),                             'Core visits (all)')
nmap_leader('vC', make_pick_core(nil),                            'Core visits (cwd)')
nmap_leader('vv', '<Cmd>lua MiniVisits.add_label("core")<CR>',    'Add "core" label')
nmap_leader('vV', '<Cmd>lua MiniVisits.remove_label("core")<CR>', 'Remove "core" label')
nmap_leader('vl', '<Cmd>lua MiniVisits.add_label()<CR>',          'Add label')
nmap_leader('vL', '<Cmd>lua MiniVisits.remove_label()<CR>',       'Remove label')

-- x is for 'Trouble'
nmap_leader('xx', '<Cmd>Trouble diagnostics toggle<CR>',                        'Diagnostics')
nmap_leader('xX', '<Cmd>Trouble diagnostics toggle filter.buf=0<CR>',          'Buffer diagnostics')
nmap_leader('xs', '<Cmd>Trouble symbols toggle focus=false<CR>',               'Symbols')
nmap_leader('xl', '<Cmd>Trouble lsp toggle focus=false win.position=right<CR>', 'LSP defs/refs')
nmap_leader('xq', '<Cmd>Trouble qflist toggle<CR>',                            'Quickfix')
nmap_leader('xQ', '<Cmd>Trouble loclist toggle<CR>',                           'Location list')
nmap_leader('xt', '<Cmd>Trouble todo toggle<CR>',                              'TODOs')
-- stylua: ignore end
