-- TODO:
-- [ ] DAP
-- [ ] Nice icons / configure lua line
-- [ ] Disable cmp before typing starts
-- [ ] ToggleTerm, maybe?
-- [ ] Fix no signature help on python code


-- Set <space> as the leader key
-- See `:help mapleader`
--  NOTE: Must happen before plugins are required (otherwise wrong leader will be used)
vim.g.mapleader = ' '
vim.g.maplocalleader = ','

-- <++> [[ Utilities ]] --
local signature_cache = {}

local function fetch_signature_async()
  local bufnr = vim.api.nvim_get_current_buf()
  local params = vim.lsp.util.make_position_params()

  vim.lsp.buf_request(bufnr, 'textDocument/signatureHelp', params, function(err, result, ctx)
    if err or not result or not result.signatures or not result.signatures[1] then
      signature_cache[bufnr] = ""
      return
    end
    signature_cache[bufnr] = result.signatures[1].label
  end)
end


-- [[ Install `lazy.nvim` plugin manager ]]
--    https://github.com/folke/lazy.nvim
--    `:help lazy.nvim.txt` for more info
local lazypath = vim.fn.stdpath 'data' .. '/lazy/lazy.nvim'
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system {
    'git',
    'clone',
    '--filter=blob:none',
    'https://github.com/folke/lazy.nvim.git',
    '--branch=stable', -- latest stable release
    lazypath,
  }
end
vim.opt.rtp:prepend(lazypath)

-- [[ Configure plugins ]]
-- NOTE: Here is where you install your plugins.
--  You can configure plugins using the `config` key.
--
--  You can also configure plugins after the setup call,
--    as they will be available in your neovim runtime.
require('lazy').setup({
  -- NOTE: First, some plugins that don't require any configuration

  -- Git related plugins
  'tpope/vim-fugitive', -- makes git commands available through :G or :Git

  -- Detect tabstop and shiftwidth automatically
  'tpope/vim-sleuth',

  -- Pluggin for installing luarocks automatically
  {
    "vhyrro/luarocks.nvim",
    priority = 1000, -- Very high priority is required, luarocks.nvim should run as the first plugin in your config.
    -- config = true,
    opts = {
      rocks = { "magick" }, -- specifies a list of rocks to install
    },
  },

  -- NOTE: This is where your plugins related to LSP can be installed.
  --  The configuration is done below. Search for lspconfig to find it below.
  {
    -- LSP Configuration & Plugins
    'neovim/nvim-lspconfig',
    dependencies = {
      -- Automatically install LSPs to stdpath for neovim
      { 'williamboman/mason.nvim',          config = true },
      { 'williamboman/mason-lspconfig.nvim' },

      -- Useful status updates for LSP (the text that appears on the lower right corner)
      -- NOTE: `opts = {}` is the same as calling `require('fidget').setup({})`
      { 'j-hui/fidget.nvim',                opts = {} },

      -- Additional lua configuration, makes nvim stuff amazing!
      'folke/neodev.nvim',
    },
  },

  -- Autocompletion
  {
    'hrsh7th/nvim-cmp',
    dependencies = {
      -- Snippet Engine & its associated nvim-cmp source
      'L3MON4D3/LuaSnip',
      'saadparwaiz1/cmp_luasnip',

      -- Adds LSP completion capabilities
      'hrsh7th/cmp-nvim-lsp',
      'hrsh7th/cmp-buffer',
      'hrsh7th/cmp-path',

      -- Adds a number of user-friendly snippets
      'rafamadriz/friendly-snippets',

      -- Copilot
      "zbirenbaum/copilot.lua",
    },
    event = "InsertEnter",
    opts = function(_, opts)
      -- Config luasnip
      require('luasnip.loaders.from_vscode').lazy_load()
      require('luasnip').config.setup({})

      -- Config copilot
      require("copilot").setup({
        suggestion = {
          enabled = true,
          auto_trigger = true,
          debounce = 150,
        }
      })

      -- Config LSP
      local cmp, copilot = require "cmp", require "copilot.suggestion"
      local snip_status_ok, luasnip = pcall(require, "luasnip")
      if not snip_status_ok then return end

      local function has_words_before()
        local line, col = unpack(vim.api.nvim_win_get_cursor(0))
        return col ~= 0 and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match "%s" == nil
      end

      local border_opts = {
        border = "rounded",
        winhighlight = "Normal:NormalFloat,FloatBorder:FloatBorder,CursorLine:PmenuSel,Search:None",
      }

      opts.formatting = {
        fields = { "kind", "abbr", "menu" },
      }

      opts.preselect = cmp.PreselectMode.None

      opts.snippet = {
        expand = function(args) luasnip.lsp_expand(args.body) end,
      }

      opts.duplicates = {
        nvim_lsp = 1,
        luasnip = 1,
        cmp_tabnine = 1,
        buffer = 1,
        path = 1,
      }

      opts.confirm_opts = {
        behavior = cmp.ConfirmBehavior.Replace,
        select = false,
      }

      opts.window = {
        completion = cmp.config.window.bordered(border_opts),
        documentation = cmp.config.window.bordered(border_opts),
      }

      if not opts.mapping then opts.mapping = {} end
      opts.mapping["<Tab>"] = cmp.mapping(function(fallback)
        if cmp.visible() then
          cmp.select_next_item()
        elseif luasnip.expand_or_jumpable() then
          luasnip.expand_or_jump()
        elseif has_words_before() then
          cmp.complete()
        else
          fallback()
        end
      end, { "i", "s" })

      opts.mapping["<C-l>"] = cmp.mapping(function()
        if copilot.is_visible() then copilot.accept() end
      end)

      opts.mapping["<C-x>"] = cmp.mapping(function()
        if copilot.is_visible() then copilot.next() end
      end)

      opts.mapping["<C-z>"] = cmp.mapping(function()
        if copilot.is_visible() then copilot.prev() end
      end)

      opts.mapping["<C-right>"] = cmp.mapping(function()
        if copilot.is_visible() then copilot.accept_word() end
      end)

      opts.mapping["<C-down>"] = cmp.mapping(function()
        if copilot.is_visible() then copilot.accept_line() end
      end)

      opts.mapping["<C-c>"] = cmp.mapping(function()
        if copilot.is_visible() then copilot.dismiss() end
      end)

      opts.mapping["<Up>"] = cmp.mapping.select_prev_item({ behavior = cmp.SelectBehavior.Select })
      opts.mapping["<Down>"] = cmp.mapping.select_next_item({ behavior = cmp.SelectBehavior.Select })
      opts.mapping["<C-p>"] = cmp.mapping.select_prev_item({ behavior = cmp.SelectBehavior.Insert })
      opts.mapping["<C-n>"] = cmp.mapping.select_next_item({ behavior = cmp.SelectBehavior.Insert })
      opts.mapping["<C-u>"] = cmp.mapping(cmp.mapping.scroll_docs(-4), { "i", "c" })
      opts.mapping["<C-d>"] = cmp.mapping(cmp.mapping.scroll_docs(4), { "i", "c" })
      opts.mapping["<C-Space>"] = cmp.mapping(cmp.mapping.complete(), { "i", "c" })
      opts.mapping["<C-y>"] = cmp.config.disable
      opts.mapping["<C-e>"] = cmp.mapping({ i = cmp.mapping.abort(), c = cmp.mapping.close() })
      opts.mapping["<CR>"] = cmp.mapping.confirm({ behavior = cmp.ConfirmBehavior.Replace, select = true })
      opts.mapping["<S-Tab>"] = cmp.mapping(function(fallback)
        if cmp.visible() then
          cmp.select_prev_item()
        elseif luasnip.jumpable(-1) then
          luasnip.jump(-1)
        else
          fallback()
        end
      end, { "i", "s" })

      opts.sources = cmp.config.sources {
        { name = "nvim_lsp", priority = 1000 },
        { name = "buffer",   priority = 750 },
        { name = "luasnip",  priority = 500 },
        { name = "path",     priority = 250 },
      }

      return opts
    end,
  },

  -- Undo tree
  { 'mbbill/undotree' },

  -- Useful plugin to show you pending keybinds.
  {
    'folke/which-key.nvim',
    event = "VeryLazy",
    opts = {
      icons = { group = "" } -- Remove + sign from groups
    }
  },
  {
    -- Adds git related signs to the gutter, as well as utilities for managing changes
    'lewis6991/gitsigns.nvim',
    opts = {
      -- See `:help gitsigns.txt`
      signs = {
        add = { text = '+' },
        change = { text = '~' },
        delete = { text = '_' },
        topdelete = { text = '‾' },
        changedelete = { text = '~' },
      },
      on_attach = function(bufnr)
        local gs = package.loaded.gitsigns

        local function map(mode, l, r, opts)
          opts = opts or {}
          opts.buffer = bufnr
          vim.keymap.set(mode, l, r, opts)
        end

        -- Navigation
        map({ 'n', 'v' }, ']c', function()
          if vim.wo.diff then
            return ']c'
          end
          vim.schedule(function()
            gs.next_hunk()
          end)
          return '<Ignore>'
        end, { expr = true, desc = 'Jump to next hunk' })

        map({ 'n', 'v' }, '[c', function()
          if vim.wo.diff then
            return '[c'
          end
          vim.schedule(function()
            gs.prev_hunk()
          end)
          return '<Ignore>'
        end, { expr = true, desc = 'Jump to previous hunk' })

        -- Actions
        -- visual mode
        map('v', '<leader>ghs', function()
          gs.stage_hunk { vim.fn.line '.', vim.fn.line 'v' }
        end, { desc = 'stage git hunk' })
        map('v', '<leader>ghr', function()
          gs.reset_hunk { vim.fn.line '.', vim.fn.line 'v' }
        end, { desc = 'reset git hunk' })
        -- normal mode
        map('n', '<leader>ghs', gs.stage_hunk, { desc = 'git stage hunk' })
        map('n', '<leader>ghr', gs.reset_hunk, { desc = 'git reset hunk' })
        map('n', '<leader>ghS', gs.stage_buffer, { desc = 'git Stage buffer' })
        map('n', '<leader>ghu', gs.undo_stage_hunk, { desc = 'undo stage hunk' })
        map('n', '<leader>ghR', gs.reset_buffer, { desc = 'git Reset buffer' })
        map('n', '<leader>ghp', gs.preview_hunk, { desc = 'preview git hunk' })
        map('n', '<leader>ghb', function()
          gs.blame_line { full = false }
        end, { desc = 'git blame line' })
        map('n', '<leader>ghd', gs.diffthis, { desc = 'git diff against index' })
        map('n', '<leader>ghD', function()
          gs.diffthis '~'
        end, { desc = 'git diff against last commit' })

        -- Toggles
        map('n', '<leader>gtb', gs.toggle_current_line_blame, { desc = 'toggle git blame line' })
        map('n', '<leader>gtd', gs.toggle_deleted, { desc = 'toggle git show deleted' })

        -- Text object
        map({ 'o', 'x' }, 'ih', ':<C-U>Gitsigns select_hunk<CR>', { desc = 'select git hunk' })
      end,
    },
  },

  {
    'nyoom-engineering/oxocarbon.nvim',
    config = function()
      -- Set theme
      vim.opt.background = "dark" -- set this to dark or light
      vim.cmd("colorscheme oxocarbon")
    end
  },

  {
    -- Set lualine as statusline
    'nvim-lualine/lualine.nvim',
    -- See `:help lualine.txt`
    opts = {
      options = {
        icons_enabled = true,
        theme = 'oxocarbon',
        component_separators = '|',
        section_separators = '',
      },
      sections = {
        lualine_c = {
          'filename',
          {
            function()
              local bufnr = vim.api.nvim_get_current_buf()
              local ts_utils = require 'nvim-treesitter.ts_utils'
              local parsers = require 'nvim-treesitter.parsers'

              if not parsers.has_parser() then return "" end

              local cursor_node = ts_utils.get_node_at_cursor()
              if not cursor_node then return signature_cache[bufnr] or "" end

              while cursor_node do
                if cursor_node:type() == 'function_call' or cursor_node:type() == 'call_expression' then
                  fetch_signature_async() -- Asynchronously update the signature
                  break
                end
                cursor_node = cursor_node:parent()
              end

              return signature_cache[bufnr] or ""
            end,
            color = { gui = "bold" }
          }
        },
      }
    },
  },

  -- Compile mode for nvim
  {
    "ej-shafran/compile-mode.nvim",
    tag = "v5.2",
    -- you can just use the latest version:
    -- branch = "latest",
    -- or the most up-to-date updates:
    -- branch = "nightly",
    dependencies = {
      "nvim-lua/plenary.nvim",
      -- if you want to enable coloring of ANSI escape codes in
      -- compilation output, add:
      { "m00qek/baleia.nvim", tag = "v1.3.0" },
    },
    config = function()
      ---@type CompileModeOpts
      vim.g.compile_mode = {
        -- to add ANSI escape code support, add:
        baleia_setup = true,
      }
    end
  },

  -- LazyGit
  {
    "kdheepak/lazygit.nvim",
    cmd = {
      "LazyGit",
      "LazyGitConfig",
      "LazyGitCurrentFile",
      "LazyGitFilter",
      "LazyGitFilterCurrentFile",
    },
    -- optional for floating window border decoration
    dependencies = {
      "nvim-lua/plenary.nvim",
    },
  },

  -- Overseer, to run tasks assyncronally
  {
    'stevearc/overseer.nvim',
    dependencies = { 'stevearc/dressing.nvim', 'rcarriga/nvim-notify' },
    config = function()
      require("overseer").setup()
    end
  },

  -- {
  --   "X3eRo0/dired.nvim",
  --   dependencies = { "MunifTanjim/nui.nvim" },
  --   config = function()
  --     -- TODO: Add keyybinds for opening current folder in other window
  --     -- TODO: make modifiable
  --     require("dired").setup({
  --       keybinds = {
  --         dired_enter = "l",
  --         dired_back = "h"
  --       }
  --     })
  --   end
  -- },

  -- Add indentation guides even on blank lines
  {
    'lukas-reineke/indent-blankline.nvim',
    main = 'ibl',
    opts = {
      indent = { char = "▏" },
      scope = { show_start = false, show_end = false },
      exclude = {
        buftypes = {
          "nofile",
          "terminal",
        },
        filetypes = {
          "help",
          "startify",
          "aerial",
          "alpha",
          "dashboard",
          "lazy",
          "neogitstatus",
          "NvimTree",
          "neo-tree",
          "Trouble",
        },
      },
    },
  },

  -- "gc" to comment visual regions/lines
  { 'numToStr/Comment.nvim', opts = {} },

  -- -- Image viewer
  -- {
  --   '3rd/image.nvim',
  --   config = function()
  --     require('image').setup({
  --       backend = 'kitty',
  --     })
  --   end
  -- },

  -- Fuzzy Finder (files, lsp, etc)
  {
    'nvim-telescope/telescope.nvim',
    branch = '0.1.x',
    dependencies = {
      'nvim-lua/plenary.nvim',
      'nvim-telescope/telescope-project.nvim',
      -- Fuzzy Finder Algorithm which requires local dependencies to be built.
      -- Only load if `make` is available. Make sure you have the system
      -- requirements installed.
      {
        'nvim-telescope/telescope-fzf-native.nvim',
        -- NOTE: If you are having trouble with this installation,
        --       refer to the README for telescope-fzf-native for more instructions.
        build = 'make',
        cond = function()
          return vim.fn.executable 'make' == 1
        end,
      },
    },
    cmd = "Telescope",
    opts = function()
      local actions = require "telescope.actions"
      local project_actions = require "telescope._extensions.project.actions"
      return {
        layout_strategy = "vertical",
        border = true,
        defaults = {
          git_worktrees = vim.g.git_worktrees,
          path_display = { "truncate" },
          sorting_strategy = "ascending",
          layout_config = {
            anchor = "S",
            prompt_position = "top",
            width = { padding = 0 },
            height = 0.3,
          },
          mappings = {
            i = {
              ["<C-n>"] = actions.cycle_history_next,
              ["<C-p>"] = actions.cycle_history_prev,
              ["<C-j>"] = actions.move_selection_next,
              ["<C-k>"] = actions.move_selection_previous,
            },
            n = { q = actions.close },
          },
        },
        extensions = {
          project = {
            base_dirs = {
              '~/Documents/GitHub',
            },
            hidden_files = true, -- default: false
            order_by = "recent",
            search_by = "title",
            on_project_selected = function(prompt_bufnr)
              -- Do anything you want in here. For example:
              project_actions.change_working_directory(prompt_bufnr, false)
            end
          }
        }
      }
    end,
  },

  -- An extension to list hunks in telecope
  {
    "radyz/telescope-gitsigns",
    config = function()
      require("telescope").load_extension("git_signs")
    end
  },

  {
    -- Highlight, edit, and navigate code
    'nvim-treesitter/nvim-treesitter',
    dependencies = {
      'nvim-treesitter/nvim-treesitter-textobjects',
    },
    build = ':TSUpdate',
  },

  -- Show header of current function on top
  { "nvim-treesitter/nvim-treesitter-context" },

  -- Format on save
  {
    'stevearc/conform.nvim',
    opts = {},
    config = function(_, opts)
      require("conform").setup({
        formatters_by_ft = {
          lua = { 'stylua', 'luacheck' },
          python = { 'isort', 'black' },
          bash = { 'shellcheck', 'shfmt' },
          go = { 'gomodifytags', 'gofumpt', 'iferr', 'impl', 'goimports' },
          markdown = { 'prettierd' },
          -- Use the "*" filetype to run formatters on all filetypes.
          ["*"] = { "codespell" },
          -- Use the "_" filetype to run formatters on filetypes that don't
          -- have other formatters configured.
          ["_"] = { "trim_whitespace" },
        },
        format_on_save = {
          -- These options will be passed to conform.format()
          timeout_ms = 500,
          lsp_fallback = true,
        },
      })
    end
  },

  -- Better yank
  {
    "AckslD/nvim-neoclip.lua",
    requires = {
      { 'nvim-telescope/telescope.nvim' },
    },
    config = function()
      require('neoclip').setup({
        keys = {
          telescope = {
            i = {
              select = '<cr>',
              paste = '<c-p>',
              paste_behind = '<c-P>',
              delete = '<c-d>', -- delete an entry
              edit = '<c-e>',   -- edit an entry
              custom = {},
            },
          },
        },
      })
    end,
  },

  -- Highlight TODOs and others
  { "folke/todo-comments.nvim",               opts = {} },

  -- vim-illuminate highlights the same words under the cursor
  {
    "RRethy/vim-illuminate",
    opts = {},
    config = function(_, opts) require("illuminate").configure(opts) end,
  },

  -- Matchup - Extend % to match keywords that start and end a code block
  {
    "andymass/vim-matchup",
    setup = function()
      vim.g.matchup_matchparen_offscreen = { method = "popup" }
      lvim.builtin.treesitter.matchup.enable = true
    end,
  },

  -- NOTE: Next Step on Your Neovim Journey: Add/Configure additional "plugins" for kickstart
  --       These are some example plugins that I've included in the kickstart repository.
  --       Uncomment any of the lines below to enable them.
  -- require 'kickstart.plugins.autoformat',
  -- require 'kickstart.plugins.debug',

  -- NOTE: The import below can automatically add your own plugins, configuration, etc from `lua/custom/plugins/*.lua`
  --    You can use this folder to prevent any conflicts with this init.lua if you're interested in keeping
  --    up-to-date with whatever is in the kickstart repo.
  --    Uncomment the following line and add your plugins to `lua/custom/plugins/*.lua` to get going.
  --
  --    For additional information see: https://github.com/folke/lazy.nvim#-structuring-your-plugins
  -- { import = 'custom.plugins' },
  --
}, {})

-- [[ Setting options ]]
-- See `:help vim.o`
-- NOTE: You can change these options as you wish!

-- Set highlight on search
vim.o.hlsearch = false

-- Make line numbers default
vim.wo.number = true

-- Enable relative line neumbers
vim.wo.relativenumber = true

-- Enable mouse mode
vim.o.mouse = 'a'

-- Sync clipboard between OS and Neovim.
--  Remove this option if you want your OS clipboard to remain independent.
--  See `:help 'clipboard'`
vim.o.clipboard = 'unnamedplus'

-- Enable break indent
vim.o.breakindent = true

-- Save undo history
vim.o.undofile = true

-- Case-insensitive searching UNLESS \C or capital in search
vim.o.ignorecase = true
vim.o.smartcase = true

-- Keep signcolumn on by default
vim.wo.signcolumn = 'yes'

-- Decrease update time
vim.o.updatetime = 250
vim.o.timeoutlen = 300

-- Set completeopt to have a better completion experience
vim.o.completeopt = 'menu,menuone,noselect'

-- NOTE: You should make sure your terminal supports this
vim.o.termguicolors = true

-- Hide command line unless needed
vim.o.cmdheight = 0

-- Copy the previous indentation on autoindenting
vim.o.copyindent = true

-- Highlight the text line of the cursor
vim.o.cursorline = true

-- Set default indent width to 2 spaces
vim.o.shiftwidth = 2

-- Set the number of spaces that a <Tab> in the file counts for
vim.o.tabstop = 2

-- Enable the use of space in tab
vim.o.expandtab = true

-- File content encoding for the buffer
-- vim.o.fileencoding = "utf-8" -- Causes error when Lazy is lauched

-- Disable `~` on nonexistent lines
-- vim.o.fillchars = { eob = " " }

-- Enable fold for nvim-ufo
vim.o.foldenable = true

-- Set high foldlevel for nvim-ufo
vim.o.foldlevel = 99

-- Start with all code unfolded
vim.o.foldlevelstart = 99

-- Show foldcolumn in nvim 0.9
vim.o.foldcolumn = vim.fn.has "nvim-0.9" == 1 and "1" or nil

-- Number of commands to remember in a history table
vim.o.history = 100

-- Global statusline
vim.o.laststatus = 3

-- Wrap lines at 'breakat'
vim.o.linebreak = true

-- Preserve indent structure as much as possible
vim.o.preserveindent = true

-- Height of the pop up menu
vim.o.pumheight = 10

-- Disable showing modes in command line
vim.o.showmode = false

-- Splitting a new window below the current one
vim.o.splitbelow = true

-- Splitting a new window at the right of the current one
vim.o.splitright = true

-- Set terminal title to the filename and path
vim.o.title = true

-- Allow going past end of line in visual block mode
vim.o.virtualedit = "block"

-- Disable wrapping of lines longer than the width of window
vim.o.wrap = false

-- Native nvim remappings
vim.keymap.set('n', 'w', 'viw') -- make w select the word under the cursor

-- [[ Highlight on yank ]]
-- See `:help vim.highlight.on_yank()`
local highlight_group = vim.api.nvim_create_augroup('YankHighlight', { clear = true })
vim.api.nvim_create_autocmd('TextYankPost', {
  callback = function()
    vim.highlight.on_yank()
  end,
  group = highlight_group,
  pattern = '*',
})

-- [[ Configure Telescope ]]
-- Enable telescope fzf native, if installed
pcall(require('telescope').load_extension, 'fzf')

-- [[ Configure Treesitter ]]
-- See `:help nvim-treesitter`
-- Defer Treesitter setup after first render to improve startup time of 'nvim {filename}'
vim.defer_fn(function()
  require('nvim-treesitter.configs').setup {
    -- Add languages to be installed here that you want installed for treesitter
    ensure_installed = { 'c', 'cpp', 'go', 'lua', 'python', 'rust', 'bash', 'latex', 'cmake', 'ninja', 'fennel', 'arduino', 'bibtex', 'markdown', 'markdown_inline', 'commonlisp', 'cuda', 'json', 'toml', 'diff', 'git_config', 'git_rebase', 'gitattributes', 'gitcommit', 'gitignore', 'scheme', 'ssh_config' },

    -- Autoinstall languages that are not installed. Defaults to false (but you can change for yourself!)
    auto_install = false,

    highlight = { enable = true },
    indent = { enable = true },
    incremental_selection = {
      enable = true,
      keymaps = {
        init_selection = '<c-space>',
        node_incremental = '<c-space>',
        scope_incremental = '<c-s>',
        node_decremental = '<M-space>',
      },
    },
    textobjects = {
      select = {
        enable = true,
        lookahead = true, -- Automatically jump forward to textobj, similar to targets.vim
        keymaps = {
          -- You can use the capture groups defined in textobjects.scm
          ['aa'] = '@parameter.outer',
          ['ia'] = '@parameter.inner',
          ['af'] = '@function.outer',
          ['if'] = '@function.inner',
          ['ac'] = '@class.outer',
          ['ic'] = '@class.inner',
        },
      },
      move = {
        enable = true,
        set_jumps = true, -- whether to set jumps in the jumplist
        goto_next_start = {
          [']m'] = '@function.outer',
          [']]'] = '@class.outer',
        },
        goto_next_end = {
          [']M'] = '@function.outer',
          [']['] = '@class.outer',
        },
        goto_previous_start = {
          ['[m'] = '@function.outer',
          ['[['] = '@class.outer',
        },
        goto_previous_end = {
          ['[M'] = '@function.outer',
          ['[]'] = '@class.outer',
        },
      },
      -- swap = {
      --   enable = true,
      --   swap_next = {
      --     ['<leader>a'] = '@parameter.inner',
      --   },
      --   swap_previous = {
      --     ['<leader>A'] = '@parameter.inner',
      --   },
      -- },
    },
  }
end, 0)

-- [[ Configure LSP ]]
--  This function gets run when an LSP connects to a particular buffer.
local on_attach = function(_, bufnr)
  -- NOTE: Remember that lua is a real programming language, and as such it is possible
  -- to define small helper and utility functions so you don't have to repeat yourself
  -- many times.
  --
  -- In this case, we create a function that lets us more easily define mappings specific
  -- for LSP related items. It sets the mode, buffer and description for us each time.
  local nmap = function(keys, func, desc)
    if desc then
      desc = 'LSP: ' .. desc
    end

    vim.keymap.set('n', keys, func, { buffer = bufnr, desc = desc })
  end

  nmap('<leader>h', vim.lsp.buf.rename, 'Rename')
  nmap('<leader>aa', vim.lsp.buf.code_action, 'Code action')
  nmap('<leader>w', function() vim.api.nvim_command('w') end, 'Save buffer')

  nmap('gd', require('telescope.builtin').lsp_definitions, 'Goto definition')
  nmap('gr', require('telescope.builtin').lsp_references, 'Goto references')
  nmap('gI', require('telescope.builtin').lsp_implementations, 'Goto implementation')
  nmap('<M-x>', require('telescope.builtin').commands, 'M-x')

  -- See `:help K` for why this keymap
  nmap('K', vim.lsp.buf.hover, 'Hover Documentation')
  nmap('<C-k>', vim.lsp.buf.signature_help, 'Signature Documentation') -- FIX: Doesn't it colide with the move window shortcuts?

  -- Lesser used LSP functionality
  nmap('gD', vim.lsp.buf.declaration, 'Goto declaration')
  -- nmap('<leader>wa', vim.lsp.buf.add_workspace_folder, '[W]orkspace [A]dd Folder')
  -- nmap('<leader>wr', vim.lsp.buf.remove_workspace_folder, '[W]orkspace [R]emove Folder')
  -- nmap('<leader>wl', function()
  --   print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
  -- end, '[W]orkspace [L]ist Folders')

  -- Create a command `:Format` local to the LSP buffer
  vim.api.nvim_buf_create_user_command(bufnr, 'Format', function(_)
    vim.lsp.buf.format()
  end, { desc = 'Format current buffer with LSP' })
end

-- document existing key chains
require('which-key').register {
  ['<leader>g'] = { name = ' Git', _ = 'which_key_ignore' },
  -- ['<leader>gh'] = { name = 'Hunk', _ = 'which_key_ignore' },
  -- ['<leader>gt'] = { name = 'Toggle', _ = 'which_key_ignore' },
  -- ['<leader>u'] = { name = '󱓍 UndoTree', _ = 'which_key_ignore' },
  -- ['<leader>f'] = { name = ' Find', _ = 'which_key_ignore' },
  -- ['<leader>c'] = { name = ' Code', _ = 'which_key_ignore' },
  -- ['<leader>e'] = { name = '󰘧 Evaluate', _ = 'which_key_ignore' },
  -- ['<leader>l'] = { name = ' Eval Buffer', _ = 'which_key_ignore' },
  -- ['<leader>d'] = { name = '󱍼 Diagnostics', _ = 'which_key_ignore' },
  -- ['<leader>a'] = { name = ' Anchor', _ = 'which_key_ignore' },
  -- ['<leader>t'] = { name = ' Tmux', _ = 'which_key_ignore' },
}
-- register which-key VISUAL mode
-- required for visual <leader>hs (hunk stage) to work
require('which-key').register({
  ['<leader>'] = { name = 'VISUAL <leader>' },
  ['<leader>c'] = { name = ' Code', _ = 'which_key_ignore' },
  ['<leader>h'] = { 'Git [H]unk' },
}, { mode = 'v' })


vim.keymap.set('n', '<leader>m', '<cmd>OverseerRun<cr>', { desc = 'Make' })


---- [[ Keymaps ]] ----

-- Keymaps for better default experience
-- See `:help vim.keymap.set()`
vim.keymap.set({ 'n', 'v' }, '<Space>', '<Nop>', { silent = true })

-- Move selection up and down
vim.keymap.set('v', "J", ":m '>+1<CR>gv=gv", { desc = "Move selection down" })
vim.keymap.set('v', "K", ":m '<-2<CR>gv=gv", { desc = "Move selection up" })

-- Remap movements
vim.keymap.set('n', "<C-h>", "<C-w>h", { desc = "Move to left split" })
vim.keymap.set('n', "<C-j>", "<C-w>j", { desc = "Move to below split" })
vim.keymap.set('n', "<C-k>", "<C-w>k", { desc = "Move to above split" })
vim.keymap.set('n', "<C-l>", "<C-w>l", { desc = "Move to right split" })

-- Remap resize
vim.keymap.set('n', "<C-Up>", "<cmd>resize -2<CR>", { desc = "Resize split up" })
vim.keymap.set('n', "<C-Down>", "<cmd>resize +2<CR>", { desc = "Resize split down" })
vim.keymap.set('n', "<C-Left>", "<cmd>vertical resize -2<CR>", { desc = "Resize split left" })
vim.keymap.set('n', "<C-Right>", "<cmd>vertical resize +2<CR>", { desc = "Resize split right" })

-- Remap for dealing with word wrap
vim.keymap.set('n', 'k', "v:count == 0 ? 'gk' : 'k'", { expr = true, silent = true })
vim.keymap.set('n', 'j', "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })

-- Indentation (Stay in indent mode)
vim.keymap.set('v', '<S-Tab>', '<gv', { desc = 'Unindent line' })
vim.keymap.set('v', '<Tab>', '>gv', { desc = 'Indent line' })

-- Escape insert mode
vim.keymap.set('i', 'jj', '<Esc>', { desc = 'Escape insert mode' })
vim.keymap.set('i', 'jk', '<Esc>', { desc = 'Escape insert mode' })

-- Git
-- vim.keymap.set('n', '<leader>gn', require('gitsigns').next_hunk, { desc = 'Jump to next hunk' })
-- vim.keymap.set('n', '<leader>gp', require('gitsigns').prev_hunk, { desc = 'Jump to previous hunk' })
vim.keymap.set('n', 'ç', require('gitsigns').next_hunk, { desc = 'Jump to next hunk' })
vim.keymap.set('n', 'Ç', require('gitsigns').prev_hunk, { desc = 'Jump to previous hunk' })
vim.keymap.set('n', '<leader>gh', "<cmd>Telescope git_signs<cr>", { desc = 'List git hunks in buffer' })
vim.keymap.set('n', '<leader>gg', "<cmd>LazyGit<cr>", { desc = 'LazyGit' })

-- UndoTree
vim.keymap.set('n', '<leader>u', vim.cmd.UndotreeToggle, { desc = 'Undo' })

-- Project management
vim.keymap.set('n', '<C-Space>', require('telescope').extensions.project.project, { desc = 'Project' })

-- Telescope
-- See `:help telescope.builtin`
vim.keymap.set('n', '<leader>f', require('telescope.builtin').find_files, { desc = 'Files' })
vim.keymap.set('n', '<leader>*', require('telescope.builtin').live_grep, { desc = 'Search Project' })
vim.keymap.set('n', '<leader>b', require('telescope.builtin').buffers, { desc = 'Buffers' })
vim.keymap.set('n', '<leader>/',
  function()
    -- You can pass additional configuration to telescope to change theme, layout, etc.
    require('telescope.builtin').current_buffer_fuzzy_find(require('telescope.themes').get_dropdown {
      winblend = 10,
      previewer = true,
    })
  end, { desc = 'Search Buffer' })
vim.keymap.set('n', '<leader>v', function()
  require("telescope.builtin").current_buffer_fuzzy_find { default_text = vim.fn.expand("<cword>") }
end, { desc = 'Find Word' })
vim.keymap.set('n', '<leader>d', require('telescope.builtin').diagnostics, { desc = 'Diagnostics' })
-- vim.keymap.set('n', '<leader>fR', require('telescope.builtin').resume, { desc = 'Find resume' })
-- vim.keymap.set('n', '<leader>ft', '<cmd>TodoTelescope keywords=TODO,FIX,WARNING,NOTE,HACK,PERF<cr>',
--   { desc = 'Find TODOs, etc' })
vim.keymap.set('n', '<leader>:', require('telescope.builtin').commands, { desc = 'M-x' })
vim.keymap.set('n', '<leader>;', ":lua ", { desc = 'Eval' })

-- Code
vim.keymap.set('v', '<leader>c', '<esc><cmd>lua require("Comment.api").toggle.linewise(vim.fn.visualmode())<cr>',
  { desc = 'Toggle Comment' })
vim.keymap.set('n', '<leader>c',
  function() require("Comment.api").toggle.linewise.count(vim.v.count > 0 and vim.v.count or 1) end,
  { desc = 'Toggle Comment' })

-- NeoClip
-- In linux, the system clipboar is the + register, but in windows it is * TODO: Fix accordingly
vim.keymap.set('n', '<leader>y', '<cmd>Telescope neoclip plus<cr>', { desc = 'Yanks' })

-- Dired
vim.keymap.set('n', '<leader>o', '<cmd>Dired<cr>', { desc = 'Dired' })

-- Split
vim.keymap.set('n', '<leader>s',
  function()
    -- Get the current window width and height
    local win_width = vim.api.nvim_win_get_width(0)
    local win_height = vim.api.nvim_win_get_height(0)

    -- Determine whether to split vertically or horizontally
    if win_width > win_height then
      vim.cmd('vsplit')
    else
      vim.cmd('split')
    end
    -- Open Telescope file picker in the new split
    require('telescope.builtin').find_files()
  end, { desc = 'Split Window' })

-- Compile mode
vim.keymap.set('n', '<leader>m', '<cmd>Compile<cr>', { desc = 'Dired' })

-- Run Async Tasks
vim.keymap.set('n', '<leader>r',
  function()
    local input = vim.fn.input("Async Run: ")
    vim.cmd("OverseerRunCmd " .. input)
    vim.cmd("OverseerToggle")
  end,
  { desc = 'Run' })
vim.keymap.set('n', '<leader>R', '<cmd>OverseerToggle<cr>', { desc = 'Toggle Run Output' })

-- <++> Anchor
-- vim.keymap.set('n', '<leader>aa', require('anchor').dropAnchor, { desc = 'Drop Anchor' })
-- vim.keymap.set('n', '<leader>aA', require('anchor').addToHistoryNoAnchor, { desc = 'Add to hist. w/o anchor' })
-- vim.keymap.set('n', '<leader>ah', require('anchor').hoistAllAnchors, { desc = 'Hoist all anchors' })
-- vim.keymap.set('n', '<leader>af', require('anchor').telescopeAnchorsInProject, { desc = 'Show anchors in project' })
-- vim.keymap.set('n', '<leader>ar', require('anchor').jumpToRecentAnchor, { desc = 'Toggle between recent anchors' })
-- vim.keymap.set('n', '<leader>aj', require('anchor').jumpToNextAnchor, { desc = 'Next anchor in buffer' })
-- vim.keymap.set('n', '<leader>ak', require('anchor').jumpToPrevAnchor, { desc = 'Previous anchor in buffer' })
-- vim.keymap.set('n', '<Tab>', require('anchor').jumpToRecentAnchor, { desc = 'Toggle between recent anchors' })
-- vim.keymap.set('n', 'ç', require('anchor').jumpToNextAnchor, { desc = 'Next anchor in buffer' })
-- vim.keymap.set('n', 'Ç', require('anchor').jumpToPrevAnchor, { desc = 'Previous anchor in buffer' })
-- vim.keymap.set('n', '<leader><leader>', function()
--   local results = {}
--   local Job = require('plenary.job')
--   Job:new({
--     command = "rg",
--     args = { "<\\+\\+>", "./" },
--     on_stdout = function(_, line)
--       table.insert(results, line)
--     end,
--   }):sync() -- Wait for the job to finish
--   if #results == 0 then
--     require('telescope.builtin').find_files()
--   else
--     require('anchor').telescopeAnchorsInProject()
--   end
-- end, { desc = 'List Anchors' })
-- vim.keymap.set('n', '+', function()
--   vim.cmd("vsplit")
--   require('anchor').jumpToNextAnchor()
--   require('anchor').addToHistoryNoAnchor()
-- end, { desc = 'Jump no next anchor in a new split' })

-- <++> Tmux
vim.keymap.set('n', '<C-h>', require("tmux").move_left, { noremap = true })
vim.keymap.set('n', '<C-j>', require("tmux").move_down, { noremap = true })
vim.keymap.set('n', '<C-k>', require("tmux").move_up, { noremap = true })
vim.keymap.set('n', '<C-l>', require("tmux").move_right, { noremap = true })
-- vim.keymap.set('n', '<leader>tt', require("tmux").list_and_select_tmux_terminals, { desc = "Tmux panes in session" })
vim.keymap.set('n', '<leader>t',
  function() require("tmux").create_or_move_tmux_pane({ split_direction = "h", focus = true }) end,
  { desc = "Terminal" })
-- vim.keymap.set('n', '<leader>tb',
--   function() require("tmux").create_or_move_tmux_pane({ split_direction = "v", focus = true }) end,
--   { desc = "New pane on the bottom" })
-- vim.keymap.set('n', '<leader>th',
--   function() require("tmux").create_or_move_tmux_pane({ split_direction = "h", pane_name = "htop", command = "htop" }) end,
--   { desc = "Pane with htop" })

---- [[ Mason/LSP ]] ----

-- mason-lspconfig requires that these setup functions are called in this order
-- before setting up the servers.
require('mason').setup()
require('mason-lspconfig').setup()

-- Enable the following language servers
--  Feel free to add/remove any LSPs that you want here. They will automatically be installed.
--
--  Add any additional override configuration in the following tables. They will be passed to
--  the `settings` field of the server config. You must look up that documentation yourself.
--
--  If you want to override the default filetypes that your language server will attach to you can
--  define the property 'filetypes' to the map in question.
local servers = {
  gopls = {},
  taplo = {},
  jsonls = {},
  bashls = {},
  texlab = {},
  clangd = {
    setup = {
      cmd = {
        "clangd",
        "--offset-encoding=utf-16",
      }
    },
  },
  -- Setolen from https://github.com/jdhao/nvim-config/blob/master/lua/config/lsp.lua
  -- And https://www.reddit.com/r/neovim/comments/tttofk/how_to_disable_annoying_pylint_warningespecially/
  -- And https://www.reddit.com/r/neovim/comments/14316t9/help_me_to_get_the_best_python_neovim_environment/
  pylsp = {
    plugins = {
      -- formatter options
      black = { enabled = true },
      autopep8 = { enabled = false },
      yapf = { enabled = false },
      -- linter options
      mcabe = { enabled = false },
      flake8 = { enabled = false, ignore = "E501,C901" },
      pylint = { enabled = true, args = { "--disable=E501", "-" } }, -- Ignore line too long warning
      ruff = { enabled = false },
      pyflakes = { enabled = false },
      pycodestyle = { enabled = false, ignore = { "E501" } }, -- Ignore line too long warning
      -- type checker
      pylsp_mypy = {
        enabled = true,
        overrides = { "--python-executable", "usr/bin/python3", true },
        report_progress = true,
        live_mode = false
      },
      -- auto-completion options
      jedi_definition = {
        enabled = true,
        follow_imports = true,
        follow_builtin_imports = true,
        follow_builtin_definitions = true,
      },
      jedi_rename = { enabled = true },
      jedi_completion = {
        enabled = true,
        fuzzy = true,
        eager = true,
        include_params = true,
        cache_labels_for = {
          "torch",
          "numpy",
          "pandas",
          "yfinance",
          "matplotlib",
          "torchvision",
        },
      },
      jedi_signature_help = { enabled = true },
      jedi_hover = { enabled = true },
      -- import sorting
      isort = { enabled = true },
    },
  },
  neocmake = {},
  marksman = {},
  rust_analyzer = {},
  fennel_language_server = {},
  arduino_language_server = {},
  lua_ls = {
    Lua = {
      workspace = { checkThirdParty = false },
      telemetry = { enable = false },
      -- NOTE: toggle below to ignore Lua_LS's noisy `missing-fields` warnings
      -- diagnostics = { disable = { 'missing-fields' } },
    },
  },
  ocamllsp = {
    enable = true,
    get_language_id = function(_, ftype)
      return ftype
    end,
  },
}

-- Setup neovim lua configuration
require('neodev').setup()

-- nvim-cmp supports additional completion capabilities, so broadcast that to servers
local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = require('cmp_nvim_lsp').default_capabilities(capabilities)

-- Ensure the servers above are installed
local mason_lspconfig = require 'mason-lspconfig'

mason_lspconfig.setup {
  ensure_installed = vim.tbl_keys(servers),
}

mason_lspconfig.setup_handlers {
  function(server_name)
    require('lspconfig')[server_name].setup {
      capabilities = capabilities,
      on_attach = on_attach,
      settings = servers[server_name],
      filetypes = (servers[server_name] or {}).filetypes,
    }
  end,
}

-- Auto-open Telescope file picker when Neovim starts with no files
vim.api.nvim_create_autocmd("VimEnter", {
  pattern = "*",
  callback = function()
    if #vim.api.nvim_list_bufs() == 1 and vim.fn.bufname() == "" and vim.fn.argc() == 0 then
      vim.defer_fn(function()
        require('telescope.builtin').find_files()
      end, 0)
    end
  end
})
