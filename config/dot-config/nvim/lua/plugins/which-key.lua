return {
  'folke/which-key.nvim',
  event = 'VeryLazy',

  config = function()
    local wk = require 'which-key'

    local opts = {
      preset = 'helix',
      debug = vim.uv.cwd():find 'which%-key',
      win = {},
      spec = {},
    }
    wk.setup(opts)

    -- Normal mode
    wk.add {
      {
        ']]',
        function()
          Snacks.words.jump(vim.v.count1)
        end,
        desc = 'Next Reference',
      },
      {
        '[[',
        function()
          Snacks.words.jump(-vim.v.count1)
        end,
        desc = 'Prev Reference',
      },

      { '<leader>b', group = 'buffer' },
      { '<leader>bb', require('telescope.builtin').buffers, desc = 'Find Buffer' },
      {
        '<leader>bd',
        function()
          Snacks.bufdelete()
        end,
        desc = 'Delete Buffer',
      },
      { '<leader>bn', '<cmd>bn<CR>', desc = 'Next Buffer' },
      { '<leader>bp', '<cmd>bp<CR>', desc = 'Prev Buffer' },
      { '<leader>bo', '<cmd>BufferLinePick<CR>', desc = 'Pick Buffer From Line' },

      { '<leader>c', group = 'code' },
      { '<leader>ca', vim.lsp.buf.code_action, desc = 'Code Action' },
      { '<leader>cd', vim.lsp.buf.definition, desc = 'Go To Definition' },
      { '<leader>cD', vim.lsp.buf.declaration, desc = 'Go to Declaration' },
      { '<leader>ci', vim.lsp.buf.implementation, desc = 'Go to Implementation' },
      { '<leader>cf', require('conform').format, desc = 'Format Buffer' },
      { '<leader>cp', '<cmd>Copilot enable<CR>', desc = 'Enable Copilot' },
      { '<leader>cP', '<cmd>Copilot disable<CR>', desc = 'Disable Copilot' },
      { '<leader>cr', vim.lsp.buf.rename, desc = 'Code Rename' },
      { '<leader>ct', vim.lsp.buf.type_definition, desc = 'Go To Type Definition' },

      { '<leader>d', group = 'diagnostics' },
      {
        '<leader>dd',
        '<cmd>Trouble diagnostics toggle<cr>',
        desc = 'Diagnostics (Trouble)',
      },
      {
        '<leader>dD',
        '<cmd>Trouble diagnostics toggle filter.buf=0<cr>',
        desc = 'Buffer Diagnostics (Trouble)',
      },

      { '<leader>f', group = 'file' },
      { '<leader>fa', '<cmd>Telescope adjacent<CR>', desc = 'Adjacent Files' },
      { '<leader>fe', '<cmd>Explore<CR>', desc = 'Explore' },
      { '<leader>ff', '<cmd>Telescope find_files hidden=true<cr>', desc = 'Find File' },
      { '<leader>fr', '<cmd>Telescope oldfiles<cr>', desc = 'Open Recent File' },

      { '<leader>g', group = 'git' },
      {
        '<leader>gb',
        function()
          Snacks.git.blame_line()
        end,
        desc = 'Current Line Blame',
      },
      {
        '<leader>gB',
        function()
          Snacks.gitbrowse()
        end,
        desc = 'Browse Git Repo Online',
      },
      {
        '<leader>gg',
        function()
          Snacks.lazygit()
        end,
        desc = 'LazyGit',
      },

      { '<leader>s', group = 'search' },
      {
        '<leader>sb',
        require('telescope.builtin').current_buffer_fuzzy_find,
        desc = 'Fuzzily search current buffer',
      },
      { '<leader>sd', require('telescope.builtin').diagnostics, desc = 'Search Diagnostics' },
      { '<leader>sd', require('telescope.builtin').diagnostics, desc = 'Search Diagnostics' },
      { '<leader>sg', require('telescope.builtin').live_grep, desc = 'Search with Grep' },
      { '<leader>sh', require('telescope.builtin').help_tags, desc = 'Search Help' },
      { '<leader>sj', require('telescope.builtin').jumplist, desc = 'Search Jumplist' },
      { '<leader>sr', require('telescope.builtin').lsp_references, desc = 'Search References' },
      { '<leader>sR', '<cmd>GrugFar<CR>', desc = 'Search and Replace' },
      { '<leader>ss', require('telescope.builtin').lsp_document_symbols, desc = 'Document Symbols' },
      { '<leader>st', '<cmd>TodoTelescope<CR>', desc = 'Search TODO' },
      { '<leader>su', '<cmd>Telescope undo<CR>', desc = 'Search Undo' },
      { '<leader>sc', require('telescope.builtin').grep_string, desc = 'Search current Word' },

      { '<leader>t', group = 'toggle' },
      { '<leader>tb', require('barbecue.ui').toggle, desc = 'Barbecue (show code context winbar)' },
      {
        '<leader>tl',
        '<cmd>Trouble lsp toggle focus=false win.position=right<cr>',
        desc = 'LSP Definitions / references / ... (Trouble)',
      },
      {
        '<leader>tL',
        '<cmd>Trouble loclist toggle<cr>',
        desc = 'Location List (Trouble)',
      },
      { '<leader>tn', '<cmd>Neotree toggle<CR>', desc = 'NeoTree' },
      {
        '<leader>ts',
        '<cmd>Trouble symbols toggle focus=false<cr>',
        desc = 'Symbols (Trouble)',
      },
      {
        '<leader>tt',
        function()
          Snacks.terminal()
        end,
        desc = 'Terminal',
      },
      {
        '<leader>tq',
        '<cmd>Trouble qflist toggle<cr>',
        desc = 'Quickfix List (Trouble)',
      },

      {
        '<leader>z',
        function()
          Snacks.notifier.hide()
        end,
        desc = 'Dismiss all notifications',
      },
    }
    -- Normal and Visual mode
    wk.add {
      mode = { 'n', 'x' },
      { '<leader>c', group = 'code' },
      {
        '<leader>cx',
        function()
          require('telescope').extensions.refactoring.refactors()
        end,
        desc = 'Show Refactors',
      },
    }
  end,
}
