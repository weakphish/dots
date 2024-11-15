return {
  -- Completion Engine
  {
    'saghen/blink.cmp',
    lazy = false, -- lazy loading handled internally
    -- optional: provides snippets for the snippet source
    dependencies = 'rafamadriz/friendly-snippets',

    -- use a release tag to download pre-built binaries
    version = 'v0.*',
    -- OR build from source, requires nightly: https://rust-lang.github.io/rustup/concepts/channels.html#working-with-nightly-rust
    -- build = 'cargo build --release',
    -- If you use nix, you can build from source using latest nightly rust with:
    -- build = 'nix run .#build-plugin',

    ---@module 'blink.cmp'
    ---@type blink.cmp.Config
    opts = {
      -- 'default' for mappings similar to built-in completion
      -- 'super-tab' for mappings similar to vscode (tab to accept, arrow keys to navigate)
      -- 'enter' for mappings similar to 'super-tab' but with 'enter' to accept
      -- see the "default configuration" section below for full documentation on how to define
      -- your own keymap.
      keymap = { preset = 'enter' },

      highlight = {
        -- sets the fallback highlight groups to nvim-cmp's highlight groups
        -- useful for when your theme doesn't support blink.cmp
        -- will be removed in a future release, assuming themes add support
        use_nvim_cmp_as_default = true,
      },
      -- set to 'mono' for 'Nerd Font Mono' or 'normal' for 'Nerd Font'
      -- adjusts spacing to ensure icons are aligned
      nerd_font_variant = 'mono',

      -- experimental auto-brackets support
      accept = { auto_brackets = { enabled = true } },

      -- experimental signature help support
      trigger = { signature_help = { enabled = true } },
    },
    -- allows extending the enabled_providers array elsewhere in your config
    -- without having to redefining it
    opts_extend = { 'sources.completion.enabled_providers' },
  },

  -- LSP servers and clients communicate what features they support through "capabilities".
  --  By default, Neovim support a subset of the LSP specification.
  --  With blink.cmp, Neovim has *more* capabilities which are communicated to the LSP servers.
  --  Explanation from TJ: https://youtu.be/m8C0Cq9Uv9o?t=1275
  --
  -- This can vary by config, but in general for nvim-lspconfig:
  {
    -- LSP Configuration & Plugins
    'neovim/nvim-lspconfig',
    opts = {
      inlay_hints = { enabled = true },
    },
    dependencies = {
      -- Automatically install LSPs to stdpath for neovim
      'williamboman/mason.nvim',
      'williamboman/mason-lspconfig.nvim',

      -- Use non-LSP as an LSP (linters, etc)
      'nvimtools/none-ls.nvim',
      'nvimtools/none-ls-extras.nvim',

      -- Useful status updates for LSP
      -- NOTE: `opts = {}` is the same as calling `require('fidget').setup({})`
      { 'j-hui/fidget.nvim', opts = {}, tag = 'legacy' },

      -- Additional lua configuration, makes nvim stuff amazing!
      'folke/neodev.nvim',

      'saghen/blink.cmp',
    },
    config = function()
      --  This function gets run when an LSP connects to a particular buffer.
      local on_attach = function(client, bufnr)
        -- Show line diagnostics in hover window
        -- Source: https://github.com/neovim/nvim-lspconfig/wiki/UI-customization#show-line-diagnostics-automatically-in-hover-window
        vim.o.updatetime = 250
        vim.cmd [[autocmd! CursorHold,CursorHoldI * lua vim.diagnostic.open_float(nil, {focus=false})]]
        vim.api.nvim_create_autocmd('CursorHold', {
          buffer = bufnr,
          callback = function()
            local opts = {
              focusable = false,
              close_events = { 'BufLeave', 'CursorMoved', 'InsertEnter', 'FocusLost' },
              border = 'rounded',
              source = 'always',
              prefix = ' ',
              scope = 'cursor',
            }
            vim.diagnostic.open_float(nil, opts)
          end,
        })
      end

      -- Enable the following language servers
      --  Feel free to add/remove any LSPs that you want here. They will automatically be installed.
      --  Add any additional override configuration in the following tables. They will be passed to
      --  the `settings` field of the server config. You must look up that documentation yourself.
      local servers = {
        clangd = {},
        gopls = {},
        pyright = {},
        ruff = {
          organizeImports = false,
        },
        rust_analyzer = {},
        ts_ls = {},
        html = {},
        lua_ls = {
          Lua = {
            workspace = { checkThirdParty = false },
            telemetry = { enable = false },
          },
        },
      }

      vim.keymap.set({ 'n' }, '<C-k>', vim.lsp.buf.signature_help, { desc = 'Signature Documentation' })

      -- Setup neovim lua configuration
      require('neodev').setup()

      -- nvim-cmp supports additional completion capabilities, so broadcast that to servers
      local capabilities = vim.lsp.protocol.make_client_capabilities()
      -- capabilities = require('cmp_nvim_lsp').default_capabilities(capabilities)
      capabilities = require('blink.cmp').get_lsp_capabilities(capabilities)

      -- Need to do GDScript manually
      require('lspconfig').gdscript.setup {}

      -- Setup mason so it can manage external tooling
      require('mason').setup()

      -- Ensure the servers above are installed
      local mason_lspconfig = require 'mason-lspconfig'

      mason_lspconfig.setup {
        ensure_installed = vim.tbl_keys(servers),
        automatic_installation = true,
      }

      mason_lspconfig.setup_handlers {
        function(server_name)
          require('lspconfig')[server_name].setup {
            capabilities = capabilities,
            on_attach = on_attach,
            settings = servers[server_name],
          }
        end,
        ['ruff'] = function()
          -- Disable Ruff's hover in favor of Pyright
          local on_attach = function(client, bufnr)
            client.server_capabilities.hoverProvider = false
          end
          require('lspconfig').ruff.setup {
            on_attach = on_attach,
          }
        end,
      }

      -- Null / None Setup
      local null_ls = require 'null-ls'

      null_ls.setup {
        sources = {
          require 'none-ls.diagnostics.eslint', -- requires none-ls-extras.nvim
          require 'null-ls.builtins.diagnostics.mypy',
          -- null_ls.builtins.completion.spell,
        },
      }
    end,
  },

  {
    -- Autopair
    'windwp/nvim-autopairs',
    event = 'InsertEnter',
    config = function()
      require('nvim-autopairs').setup {}
    end,
  },

  -- Auto tag closing
  {
    'windwp/nvim-ts-autotag',
    config = function()
      require('nvim-ts-autotag').setup()
    end,
  },

  -- Formatter management
  {
    'stevearc/conform.nvim',
    opts = {},
    config = function()
      require('conform').setup {
        formatters_by_ft = {
          lua = { 'stylua' },
          -- Conform will run multiple formatters sequentially
          python = { 'isort', 'black' },
          javascript = { 'prettier' },
          typescript = { 'prettier' },
          markdown = { 'markdownlint' },
          go = { 'gofmt', 'goimports' },
        },
      }
    end,
  },

  -- Beep Boop write code for me
  {
    'github/copilot.vim',
    config = function()
      vim.g.copilot_no_tab_map = true
      vim.g.copilot_filetypes = { markdown = false }
      vim.api.nvim_set_keymap('i', '<C-J>', 'copilot#Accept("<CR>")', { silent = true, expr = true })
    end,
  },

  -- Visualize code actions
  {
    'rachartier/tiny-code-action.nvim',
    dependencies = {
      { 'nvim-lua/plenary.nvim' },
      { 'nvim-telescope/telescope.nvim' },
    },
    event = 'LspAttach',
    config = function()
      require('tiny-code-action').setup()
    end,
  },
}
