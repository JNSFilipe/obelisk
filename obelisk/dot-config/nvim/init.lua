-- TODO:
-- [ ] DAP
-- [ ] Configure lua line
-- [ ] Disable cmp before typing starts
-- [ ] Fix no signature help on python code

-- Set <space> as the leader key
-- See `:help mapleader`
--  NOTE: Must happen before plugins are required (otherwise wrong leader will be used)
vim.g.mapleader = ' '
vim.g.maplocalleader = ','

-- [[ Utilities ]] --
-- Function to search for a Makefile in the current directory
local function find_makefile()
  local makefile_paths = { "Makefile", "makefile", "Makefile.in" }
  for _, file in ipairs(makefile_paths) do
    if vim.fn.filereadable(file) == 1 then
      return file
    end
  end
  return nil
end

-- Function to parse make targets from the Makefile
local function parse_make_targets(makefile)
  local targets = {}
  -- local handle = io.popen("make -p -f " .. makefile .. " 2>/dev/null | awk '/^([^:]+):/ {print $1}'")
  local handle = io.popen("cat " .. makefile .. " | grep '^[^#[:space:]].*:' | sed 's/:.*//'")
  for line in handle:lines() do
    if line ~= ".PHONY" then
      table.insert(targets, line)
    end
  end
  handle:close()
  return targets
end

local function make_targets(opts)
  local pickers = require("telescope.pickers")
  local finders = require("telescope.finders")
  local conf = require("telescope.config").values
  local actions = require("telescope.actions")
  local action_state = require("telescope.actions.state")
  local compile_mode = require("compile-mode")

  local makefile = find_makefile()
  if not makefile then
    print("No Makefile found in the current directory.")
    return
  end

  local targets = parse_make_targets(makefile)
  if #targets == 0 then
    print("No targets found in the Makefile.")
    return
  end
  vim.inspect(targets)

  opts = opts or {}
  pickers
      .new(opts, {
        prompt_title = "commands",
        finder = finders.new_table(targets),
        sorter = conf.generic_sorter(opts),
        attach_mappings = function(prompt_bufnr, map)
          actions.select_default:replace(function()
            actions.close(prompt_bufnr)
            local selection = action_state.get_selected_entry()
            local command = "make -k " .. selection[1]
            compile_mode.compile({ args = command, smods = {} })
          end)
          return true
        end,
      })
      :find()
end

local function tmux_move_left()
  if vim.fn.winnr('h') ~= vim.fn.winnr() then
    vim.cmd('wincmd h')
  else
    vim.cmd('silent !tmux select-pane -L')
  end
end

local function tmux_move_down()
  if vim.fn.winnr('j') ~= vim.fn.winnr() then
    vim.cmd('wincmd j')
  else
    vim.cmd('silent !tmux select-pane -D')
  end
end

local function tmux_move_up()
  if vim.fn.winnr('k') ~= vim.fn.winnr() then
    vim.cmd('wincmd k')
  else
    vim.cmd('silent !tmux select-pane -U')
  end
end

local function tmux_move_right()
  if vim.fn.winnr('l') ~= vim.fn.winnr() then
    vim.cmd('wincmd l')
  else
    vim.cmd('silent !tmux select-pane -R')
  end
end

-- [[ Install `lazy.nvim` plugin manager ]]
--    https://github.com/folke/lazy.nvim
--    `:help lazy.nvim.txt` for more info
-- Bootstrap lazy.nvim
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not (vim.uv or vim.loop).fs_stat(lazypath) then
  local lazyrepo = "https://github.com/folke/lazy.nvim.git"
  local out = vim.fn.system({ "git", "clone", "--filter=blob:none", "--branch=stable", lazyrepo, lazypath })
  if vim.v.shell_error ~= 0 then
    vim.api.nvim_echo({
      { "Failed to clone lazy.nvim:\n", "ErrorMsg" },
      { out,                            "WarningMsg" },
      { "\nPress any key to exit..." },
    }, true, {})
    vim.fn.getchar()
    os.exit(1)
  end
end
vim.opt.rtp:prepend(lazypath)

-- Make sure to setup `mapleader` and `maplocalleader` before
-- loading lazy.nvim so that mappings are correct.
-- This is also a good place to setup other settings (vim.opt)
vim.g.mapleader = " "
vim.g.maplocalleader = "\\"

-- require('maker').pick_make_target()

-- [[ Configure plugins ]]
require('lazy').setup({

  -- Git related plugins
  'tpope/vim-fugitive', -- makes git commands available through :G or :Git

  -- Detect tabstop and shiftwidth automatically
  'tpope/vim-sleuth',

  -- For lua stuff
  {
    "folke/lazydev.nvim",
    ft = "lua", -- only load on lua files
    opts = {
      library = {
        -- See the configuration section for more details
        -- Load luvit types when the `vim.uv` word is found
        { path = "luvit-meta/library", words = { "vim%.uv" } },
      },
    },
  },
  { "Bilal2453/luvit-meta", lazy = true }, -- optional `vim.uv` typings

  -- Snippeds, LSP, and CMP
  -- github.com/ThePrimeagen/init.lua/blob/master/lua/theprimeagen/lazy/lsp.lua
  -- github.com/ThePrimeagen/init.lua/blob/master/lua/theprimeagen/lazy/snippets.lua
  {
    "L3MON4D3/LuaSnip",
    -- follow latest release.
    version = "v2.*", -- Replace <CurrentMajor> by the latest released major (first number of latest release)
    -- install jsregexp (optional!).
    build = "make install_jsregexp",

    dependencies = { "rafamadriz/friendly-snippets" },

    config = function()
      local ls = require("luasnip")
      ls.filetype_extend("javascript", { "jsdoc" })

      --- TODO: What is expand?
      vim.keymap.set({ "i" }, "<C-s>e", function() ls.expand() end, { silent = true })

      vim.keymap.set({ "i", "s" }, "<C-s>;", function() ls.jump(1) end, { silent = true })
      vim.keymap.set({ "i", "s" }, "<C-s>,", function() ls.jump(-1) end, { silent = true })

      vim.keymap.set({ "i", "s" }, "<C-E>", function()
        if ls.choice_active() then
          ls.change_choice(1)
        end
      end, { silent = true })
    end,
  },
  {
    "neovim/nvim-lspconfig",
    dependencies = {
      "williamboman/mason.nvim",
      "williamboman/mason-lspconfig.nvim",
      "hrsh7th/cmp-nvim-lsp",
      "hrsh7th/cmp-buffer",
      "hrsh7th/cmp-path",
      "hrsh7th/cmp-cmdline",
      "hrsh7th/nvim-cmp",
      "L3MON4D3/LuaSnip",
      "saadparwaiz1/cmp_luasnip",
      "j-hui/fidget.nvim",
    },

    config = function()
      local cmp = require('cmp')
      local cmp_lsp = require("cmp_nvim_lsp")
      local capabilities = vim.tbl_deep_extend(
        "force",
        {},
        vim.lsp.protocol.make_client_capabilities(),
        cmp_lsp.default_capabilities())

      require("fidget").setup({})
      require("mason").setup()
      require("mason-lspconfig").setup({
        ensure_installed = {
          "lua_ls",
          "rust_analyzer",
          "gopls",
          "clangd",
          "texlab",
          "bashls",
          "pylsp",
          -- "neocmake",
          "arduino_language_server",
        },
        handlers = {
          function(server_name) -- default handler (optional)
            require("lspconfig")[server_name].setup {
              capabilities = capabilities
            }
          end,

          zls = function()
            local lspconfig = require("lspconfig")
            lspconfig.zls.setup({
              root_dir = lspconfig.util.root_pattern(".git", "build.zig", "zls.json"),
              settings = {
                zls = {
                  enable_inlay_hints = true,
                  enable_snippets = true,
                  warn_style = true,
                },
              },
            })
            vim.g.zig_fmt_parse_errors = 0
            vim.g.zig_fmt_autosave = 0
          end,
          ["lua_ls"] = function()
            local lspconfig = require("lspconfig")
            lspconfig.lua_ls.setup {
              capabilities = capabilities,
              settings = {
                Lua = {
                  runtime = { version = "Lua 5.1" },
                  diagnostics = {
                    globals = { "bit", "vim", "it", "describe", "before_each", "after_each" },
                  }
                }
              }
            }
          end,
          -- Setolen from https://github.com/jdhao/nvim-config/blob/master/lua/config/lsp.lua
          -- And https://www.reddit.com/r/neovim/comments/tttofk/how_to_disable_annoying_pylint_warningespecially/
          -- And https://www.reddit.com/r/neovim/comments/14316t9/help_me_to_get_the_best_python_neovim_environment/
          ["pylsp"] = function()
            local lspconfig = require("lspconfig")
            lspconfig.pylsp.setup {
              capabilities = capabilities,
              settings = {
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
                }
              }
            }
          end,
        }
      })

      local cmp_select = { behavior = cmp.SelectBehavior.Select }

      cmp.setup({
        snippet = {
          expand = function(args)
            require('luasnip').lsp_expand(args.body) -- For `luasnip` users.
          end,
        },
        mapping = cmp.mapping.preset.insert({
          ['<C-k>'] = cmp.mapping.select_prev_item(cmp_select),
          ['<C-j>'] = cmp.mapping.select_next_item(cmp_select),
          ['<Tab>'] = cmp.mapping.confirm({ select = true }),
          ['<cr>'] = cmp.mapping.confirm({ select = true }),
          ["<C-ç>"] = cmp.mapping.complete(),
        }),
        sources = cmp.config.sources({
          { name = 'nvim_lsp' },
          { name = 'luasnip' }, -- For luasnip users.
        }, {
          { name = 'buffer' },
        })
      })

      vim.diagnostic.config({
        -- update_in_insert = true,
        float = {
          focusable = false,
          style = "minimal",
          border = "rounded",
          source = "always",
          header = "",
          prefix = "",
        },
      })

      vim.api.nvim_create_autocmd('LspAttach', {
        callback = function(e)
          local opts = { buffer = e.buf }
          vim.keymap.set("n", "gd", function() vim.lsp.buf.definition() end, opts)
          vim.keymap.set("n", "gD", function() vim.lsp.buf.declaration() end, opts)
          vim.keymap.set("n", "gi", function() vim.lsp.buf.implementation() end, opts)
          vim.keymap.set("n", "go", function() vim.lsp.buf.type_definition() end, opts)
          vim.keymap.set("n", "gr", function() vim.lsp.buf.references() end, opts)
          vim.keymap.set("n", "K", function() vim.lsp.buf.hover() end, opts)
          vim.keymap.set("n", "<leader>a", function() vim.lsp.buf.code_action() end, opts) -- TODO: Add description
          vim.keymap.set("n", "H", function() vim.lsp.buf.signature_help() end, opts)
          vim.keymap.set("i", "<C-h>", function() vim.lsp.buf.signature_help() end, opts)
        end
      })
    end
  },

  -- Fuzzy Finder (files, lsp, etc)
  {
    'nvim-telescope/telescope.nvim',
    branch = '0.1.x',
    lazy = false,
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
        cond = function() return vim.fn.executable 'make' == 1 end,
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
          order_by = "recent",
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

  -- An extension to sort telescope results by recency
  {
    'prochri/telescope-all-recent.nvim',
    dependencies = {
      "nvim-telescope/telescope.nvim",
      "kkharji/sqlite.lua",
      -- optional, if using telescope for vim.ui.select
      "stevearc/dressing.nvim"
    },
    opts =
    {
      -- your config goes here
    }
  },

  -- Useful plugin to show you pending keybinds.
  {
    'folke/which-key.nvim',
    event = "VeryLazy",
    dependencies = {
      "nvim-telescope/telescope.nvim",
      "nvim-tree/nvim-web-devicons",
      "echasnovski/mini.icons",
    },
    opts = {
      icons = { group = "" } -- Remove + sign from groups
    },
    keys = {
      -- Git
      { "<leader>g", "<cmd>LazyGit<cr>",              desc = 'LazyGit' },
      { "<leader>G", "<cmd>Telescope git_signs<cr>",  desc = 'List git hunks in buffer' },

      -- UndoTree
      { '<leader>u', vim.cmd.UndotreeToggle,          desc = 'Undo' },

      -- Telescope related
      { '<leader>f', "<cmd>Telescope find_files<cr>", desc = 'Files' },
      { '<leader>*', "<cmd>Telescope live_grep<cr>",  desc = 'Search Project' },
      { '<leader>b', "<cmd>Telescope buffers<cr>",    desc = 'Buffers' },
      { '<leader>H', "<cmd>Telescope man_pages<cr>",  desc = 'Man Pages' },
      {
        '<leader>/',
        function()
          -- You can pass additional configuration to telescope to change theme, layout, etc.
          require('telescope.builtin').current_buffer_fuzzy_find(require('telescope.themes').get_dropdown {
            winblend = 10,
            previewer = true,
          })
        end,
        desc = 'Search Buffer'
      },
      {
        '<leader>v',
        function()
          require("telescope.builtin").current_buffer_fuzzy_find { default_text = vim.fn.expand("<cword>") }
        end,
        desc = 'Find Word'
      },
      { '<leader>d', "<cmd>Telescope diagnostics<cr>",  desc = 'Diagnostics' },
      { '<leader>:', "<cmd>Telescope commands<cr>",     desc = 'M-x' },

      -- NeoClip
      -- In linux, the system clipboar is the + register, but in windows it is * TODO: Fix accordingly
      { '<leader>y', '<cmd>Telescope neoclip plus<cr>', desc = 'Yanks' },

      -- Oil
      { '<leader>o', '<cmd>Oil<cr>',                    desc = 'File Explorer' },

      -- Split
      {
        '<leader>s',
        function()
          -- Get the current window width and height
          local win_width = vim.api.nvim_win_get_width(0)
          local win_height = vim.api.nvim_win_get_height(0) * 2.2

          -- Determine whether to split vertically or horizontally
          if win_width > win_height then
            vim.cmd('vsplit')
          else
            vim.cmd('split')
          end
          -- Open Telescope file picker in the new split
          require('telescope.builtin').find_files()
        end,
        desc = 'Split Window'
      },
      {
        '<leader>x',
        function()
          vim.cmd('only')
        end,
        desc = 'Monocle'
      },

      -- Compile
      { '<leader>m', make_targets,                                           desc = 'Make' },
      { '<leader>M', '<cmd>Compile<cr>',                                     desc = 'Compile' },

      -- Misc
      { '<leader>w', "<cmd>w<cr>",                                           desc = 'Save Buffer' },
      { '<leader>h', [[:%s/\<<C-r><C-w>\>/<C-r><C-w>/gI<Left><Left><Left>]], desc = 'Replace Word' },
      { '<leader>;', ":lua ",                                                desc = 'Eval' },

    },
  },

  -- Undo tree
  { 'mbbill/undotree' },

  -- File Explorer
  {
    'stevearc/oil.nvim',
    ---@module 'oil'
    ---@type oil.SetupOpts
    opts = {},
    -- Optional dependencies
    dependencies = { { "echasnovski/mini.icons", opts = {} } },
    -- dependencies = { "nvim-tree/nvim-web-devicons" }, -- use if prefer nvim-web-devicons
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
    },
  },

  -- Theme
  {
    'nyoom-engineering/oxocarbon.nvim',
    config = function()
      -- Set theme
      vim.opt.background = "dark" -- set this to dark or light
      vim.cmd("colorscheme oxocarbon")
    end
  },

  -- -- lsp-status.nvim for statusline integration
  -- {
  --   "nvim-lua/lsp-status.nvim",
  --   config = function()
  --     local lsp_status = require('lsp-status')
  --
  --     -- Configure lsp-status
  --     lsp_status.config({
  --       status_symbol = " ", -- Symbol for the LSP status, change as needed
  --       indicator_ok = "✓", -- Indicator when LSP is connected and working
  --       indicator_hint = "", -- Indicator for hints
  --       indicator_info = "", -- Indicator for info
  --       indicator_warn = "", -- Indicator for warnings
  --       indicator_error = "", -- Indicator for errors
  --     })
  --
  --     -- Register lsp-status to handle LSP progress
  --     lsp_status.register_progress()
  --   end
  -- },

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
            -- function()
            --   local bufnr = vim.api.nvim_get_current_buf()
            --   local ts_utils = require 'nvim-treesitter.ts_utils'
            --   local parsers = require 'nvim-treesitter.parsers'
            --
            --   if not parsers.has_parser() then return "" end
            --
            --   local cursor_node = ts_utils.get_node_at_cursor()
            --   if not cursor_node then return signature_cache[bufnr] or "" end
            --
            --   while cursor_node do
            --     if cursor_node:type() == 'function_call' or cursor_node:type() == 'call_expression' then
            --       fetch_signature_async() -- Asynchronously update the signature
            --       break
            --     end
            --     cursor_node = cursor_node:parent()
            --   end
            --
            --   return signature_cache[bufnr] or ""
            -- end,
          }
        },
      }
    },
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

  {
    "ej-shafran/compile-mode.nvim",
    -- tag = "v5.*",
    -- you can just use the latest version:
    -- branch = "latest",
    -- or the most up-to-date updates:
    branch = "nightly",
    dependencies = {
      "nvim-lua/plenary.nvim",
      -- if you want to enable coloring of ANSI escape codes in
      -- compilation output, add:
      { "m00qek/baleia.nvim", tag = "v1.3.0" },
    },
    config = function()
      ---@type CompileModeOpts
      vim.g.compile_mode = {
        buffer_name = "compilation",
        -- to add ANSI escape code support, add:
        baleia_setup = true,
      }
    end
  },

  -- -- Overseer, to run tasks assyncronally
  -- {
  --   'stevearc/overseer.nvim',
  --   -- dependencies = { 'stevearc/dressing.nvim', 'rcarriga/nvim-notify' },
  --   config = function()
  --     require("overseer").setup()
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
  { 'numToStr/Comment.nvim',                  opts = {} },

  -- Highlight, edit, and navigate code
  {
    'nvim-treesitter/nvim-treesitter',
    dependencies = {
      'nvim-treesitter/nvim-treesitter-textobjects',
      { 'nushell/tree-sitter-nu', build = ':TSUpdate nu' },
    },
    build = ':TSUpdate',
    config = function()
      local configs = require("nvim-treesitter.configs")

      configs.setup({
        ensure_installed = { 'vimdoc', 'c', 'cpp', 'go', 'lua', 'python', 'rust', 'bash', 'latex', 'cmake', 'ninja', 'fennel', 'arduino', 'bibtex', 'markdown', 'markdown_inline', 'commonlisp', 'cuda', 'json', 'toml', 'diff', 'git_config', 'git_rebase', 'gitattributes', 'gitcommit', 'gitignore', 'scheme', 'ssh_config', 'tmux', 'nu' },
        sync_install = false,
        auto_install = true,
        highlight = { enable = true, additional_vim_regex_highlighting = false },
        indent = { enable = true },
      })
    end
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
  { "folke/todo-comments.nvim", opts = {} },

  -- vim-illuminate highlights the same words under the cursor
  {
    "RRethy/vim-illuminate",
    opts = {
      delay = 200,
      large_file_cutoff = 2000,
    },
    config = function(_, opts) require("illuminate").configure(opts) end,
  },

}, {})

-- [[ Setting options ]]
-- See `:help vim.o`
-- NOTE: You can change these options as you wish!

-- Set highlight on search
vim.o.hlsearch = false
vim.o.incsearch = true

-- Make line numbers default
vim.wo.number = true

-- Ensure that there are at least 8 lines bellow/above teh cursor when possible
vim.o.scrolloff = 8

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
vim.o.swapfile = false
vim.o.backup = false
vim.o.undodir = os.getenv("HOME") ..
    "/.vim/undodir" -- FIX: This makes undo history available to undodir, but I don't think it is working
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
vim.keymap.set('n', 'W', 'viW') -- make W select the word under the cursor

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

-- -- [[ Documentation ]]
-- -- Function to show hover documentation in the echo area
-- vim.lsp.handlers["textDocument/hover"] = function(_, result, ctx, _)
--   if not (result and result.contents) then return end
--
--   local contents = result.contents
--   local lines = {}
--
--   if type(contents) == "table" then
--     for _, content in ipairs(contents) do
--       if type(content) == "table" and content.value then
--         table.insert(lines, content.value)
--       end
--     end
--   else
--     table.insert(lines, contents)
--   end
--
--   -- Join the lines and display in the echo area
--   vim.api.nvim_echo({ { table.concat(lines, "\n"), "Normal" } }, true, {})
-- end

---- [[ Keymaps ]] ----

-- Keymaps for better default experience
-- See `:help vim.keymap.set()`
vim.keymap.set({ 'n', 'v' }, '<Space>', '<Nop>', { silent = true })

-- Move selection up and down
vim.keymap.set('v', "J", ":m '>+1<CR>gv=gv", { desc = "Move selection down" })
vim.keymap.set('v', "K", ":m '<-2<CR>gv=gv", { desc = "Move selection up" })

-- Put search terms in the middle of the screen
vim.keymap.set('v', "n", "nzzzv")
vim.keymap.set('v', "N", "Nzzzv")

-- Bind C-c to esc, like ThePrimeagen... seems promessing
vim.keymap.set('i', "<C-c>", "<Esc>")

-- Disable macros in Q
vim.keymap.set('n', "Q", "<nop>")

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

-- Jump to previous position and back
vim.keymap.set('n', '<S-Tab>', '<C-i>', { desc = 'Jump to next position' })
vim.keymap.set('n', '<Tab>', '<C-o>', { desc = 'Jump to previous position' })

-- Escape insert mode
vim.keymap.set('i', 'jj', '<Esc>', { desc = 'Escape insert mode' })
vim.keymap.set('i', 'jk', '<Esc>', { desc = 'Escape insert mode' })

-- Git
vim.keymap.set('n', 'ç', function() require('gitsigns').nav_hunk('next') end, { desc = 'Jump to next hunk' })
-- FIX: uppercase ç (Ç) is not working
vim.keymap.set('n', 'Ç', function() require('gitsigns').nav_hunk('prev') end, { desc = 'Jump to previous hunk' })

-- Code
vim.keymap.set('v', '<leader>c', '<esc><cmd>lua require("Comment.api").toggle.linewise(vim.fn.visualmode())<cr>',
  { desc = 'Toggle Comment' })
vim.keymap.set('n', '<leader>c',
  function() require("Comment.api").toggle.linewise.count(vim.v.count > 0 and vim.v.count or 1) end,
  { desc = 'Toggle Comment' })

-- <++> Tmux
vim.keymap.set('n', '<C-h>', tmux_move_left, { noremap = true })
vim.keymap.set('n', '<C-j>', tmux_move_down, { noremap = true })
vim.keymap.set('n', '<C-k>', tmux_move_up, { noremap = true })
vim.keymap.set('n', '<C-l>', tmux_move_right, { noremap = true })

-- Close window
vim.keymap.set('n', '<C-q>', '<cmd>clo<cr>', { desc = 'Close Window' })

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
