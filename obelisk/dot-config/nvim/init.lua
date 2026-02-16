-- TODO:
-- [ ] Configure lua line
-- [ ] Disable cmp before typing starts
-- [ ] Fix no signature help on python code

-----------------------------------------------------------------------
-- Leader keys (set once, early)
-----------------------------------------------------------------------
vim.g.mapleader = " "
vim.g.maplocalleader = ","

-- If you have a Nerd Font, set true for nicer diagnostic icons
vim.g.have_nerd_font = true

-----------------------------------------------------------------------
-- Utilities
-----------------------------------------------------------------------
local function in_tmux()
	local t = os.getenv("TMUX")
	return t ~= nil and t ~= ""
end

local function tmux_cmd(args)
	if not in_tmux() then
		return
	end
	if vim.fn.executable("tmux") == 0 then
		return
	end
	vim.cmd("silent !tmux " .. args)
end

local function tmux_move_left()
	if vim.fn.winnr("h") ~= vim.fn.winnr() then
		vim.cmd("wincmd h")
	else
		tmux_cmd("select-pane -L")
	end
end

local function tmux_move_down()
	if vim.fn.winnr("j") ~= vim.fn.winnr() then
		vim.cmd("wincmd j")
	else
		tmux_cmd("select-pane -D")
	end
end

local function tmux_move_up()
	if vim.fn.winnr("k") ~= vim.fn.winnr() then
		vim.cmd("wincmd k")
	else
		tmux_cmd("select-pane -U")
	end
end

local function tmux_move_right()
	if vim.fn.winnr("l") ~= vim.fn.winnr() then
		vim.cmd("wincmd l")
	else
		tmux_cmd("select-pane -R")
	end
end

local function ido_open_or_fallback(opts, fallback)
	local ok, mod = pcall(require, "ido")
	if ok and mod and type(mod.open) == "function" then
		local ok_open, err = pcall(mod.open, opts or {})
		if ok_open then
			return
		end
		vim.notify("Ido picker error: " .. tostring(err), vim.log.levels.ERROR)
	else
		vim.notify("Ido picker not available, falling back", vim.log.levels.WARN)
	end
	if fallback then
		fallback()
	end
end

-----------------------------------------------------------------------
-- Bootstrap lazy.nvim
-----------------------------------------------------------------------
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not (vim.uv or vim.loop).fs_stat(lazypath) then
	local lazyrepo = "https://github.com/folke/lazy.nvim.git"
	local out = vim.fn.system({ "git", "clone", "--filter=blob:none", "--branch=stable", lazyrepo, lazypath })
	if vim.v.shell_error ~= 0 then
		vim.api.nvim_echo({
			{ "Failed to clone lazy.nvim:\n", "ErrorMsg" },
			{ out, "WarningMsg" },
			{ "\nPress any key to exit..." },
		}, true, {})
		vim.fn.getchar()
		os.exit(1)
	end
end
vim.opt.rtp:prepend(lazypath)

-----------------------------------------------------------------------
-- Plugins
-----------------------------------------------------------------------
require("lazy").setup({

	{ "nvim-tree/nvim-web-devicons", lazy = true },

	---------------------------------------------------------------------
	-- Lua dev helpers
	---------------------------------------------------------------------
	{
		"folke/lazydev.nvim",
		ft = "lua",
		opts = {
			library = {
				{ path = "${3rd}/luv/library", words = { "vim%.uv" } },
			},
		},
	},

	---------------------------------------------------------------------
	-- LSP (Neovim 0.11+ native config/enable)
	---------------------------------------------------------------------
	{
		"neovim/nvim-lspconfig",
		dependencies = {
			{ "mason-org/mason.nvim", opts = {} },
			{ "mason-org/mason-lspconfig.nvim", opts = {} },
			{ "WhoIsSethDaniel/mason-tool-installer.nvim" },
			{ "j-hui/fidget.nvim", opts = {} },
			"saghen/blink.cmp",
		},
		config = function()
			vim.api.nvim_create_autocmd("LspAttach", {
				group = vim.api.nvim_create_augroup("user-lsp-attach", { clear = true }),
				callback = function(event)
					local map = function(keys, func, desc, mode)
						mode = mode or "n"
						vim.keymap.set(mode, keys, func, { buffer = event.buf, desc = "LSP: " .. desc })
					end

					map("grn", vim.lsp.buf.rename, "[R]e[n]ame")
					map("gra", vim.lsp.buf.code_action, "Code [A]ction", { "n", "x" })

					-- Direct jump to definition
					map("gd", vim.lsp.buf.definition, "Go to [D]definition")
					map("grr", function()
						Snacks.picker.lsp_references()
					end, "[R]eferences")
					map("gri", function()
						Snacks.picker.lsp_implementations()
					end, "[I]mplementation")
					map("grd", function()
						Snacks.picker.lsp_definitions()
					end, "[D]definition")
					map("grD", vim.lsp.buf.declaration, "[D]eclaration")
					map("grt", function()
						Snacks.picker.lsp_type_definitions()
					end, "[T]ype Definition")

					map("gO", function()
						Snacks.picker.lsp_symbols()
					end, "Document Symbols")
					map("gW", function()
						Snacks.picker.lsp_symbols({ workspace = true })
					end, "Workspace Symbols")

					-- Signature help fallback in insert mode
					map("<C-k>", vim.lsp.buf.signature_help, "Signature Help", "i")

					local client = vim.lsp.get_client_by_id(event.data.client_id)
					if not client then
						return
					end

					if client.server_capabilities.documentHighlightProvider then
						local aug = vim.api.nvim_create_augroup("user-lsp-highlight", { clear = false })

						vim.api.nvim_create_autocmd({ "CursorHold", "CursorHoldI" }, {
							buffer = event.buf,
							group = aug,
							callback = vim.lsp.buf.document_highlight,
						})

						vim.api.nvim_create_autocmd({ "CursorMoved", "CursorMovedI" }, {
							buffer = event.buf,
							group = aug,
							callback = vim.lsp.buf.clear_references,
						})

						vim.api.nvim_create_autocmd("LspDetach", {
							group = vim.api.nvim_create_augroup("user-lsp-detach", { clear = true }),
							callback = function(ev)
								vim.lsp.buf.clear_references()
								vim.api.nvim_clear_autocmds({ group = "user-lsp-highlight", buffer = ev.buf })
							end,
						})
					end

					if client.server_capabilities.inlayHintProvider then
						map("<leader>th", function()
							local enabled = vim.lsp.inlay_hint.is_enabled({ bufnr = event.buf })
							vim.lsp.inlay_hint.enable(not enabled, { bufnr = event.buf })
						end, "[T]oggle Inlay [H]ints")
					end
				end,
			})

			vim.diagnostic.config({
				severity_sort = true,
				float = { border = "rounded", source = "if_many" },
				underline = { severity = vim.diagnostic.severity.ERROR },
				signs = vim.g.have_nerd_font and {
					text = {
						[vim.diagnostic.severity.ERROR] = "󰅚 ",
						[vim.diagnostic.severity.WARN] = "󰀪 ",
						[vim.diagnostic.severity.INFO] = "󰋽 ",
						[vim.diagnostic.severity.HINT] = "󰌶 ",
					},
				} or {},
				virtual_text = {
					source = "if_many",
					spacing = 2,
					format = function(d)
						return d.message
					end,
				},
			})

			-- Capabilities (blink.cmp)
			local caps = vim.lsp.protocol.make_client_capabilities()
			local ok, blink = pcall(require, "blink.cmp")
			if ok then
				caps = blink.get_lsp_capabilities(caps)
			end
			vim.lsp.config("*", { capabilities = caps })

			-- Server configs
			vim.lsp.config("lua_ls", {
				settings = {
					Lua = {
						completion = { callSnippet = "Replace" },
						diagnostics = { globals = { "vim", "it", "describe", "before_each", "after_each" } },
					},
				},
			})

			vim.lsp.config("clangd", {
				cmd = {
					"clangd",
					"--background-index",
					"--clang-tidy",
					"--header-insertion=iwyu",
					"--completion-style=detailed",
					"--function-arg-placeholders",
					"--fallback-style=llvm",
				},
				init_options = {
					usePlaceholders = true,
					completeUnimported = true,
					clangdFileStatus = true,
				},
				root_markers = { "compile_commands.json", "compile_flags.txt", ".git", "Makefile" },
			})
			vim.lsp.config("ty", {
				cmd = { "ty", "server" },
				filetypes = { "python" },
				root_markers = { "pyproject.toml", "setup.py", "setup.cfg", "requirements.txt", ".git" },
			})

			vim.lsp.config("zls", {
				root_markers = { ".git", "build.zig", "zls.json" },
				settings = { zls = { enable_inlay_hints = true, enable_snippets = true, warn_style = true } },
			})
			vim.g.zig_fmt_parse_errors = 0
			vim.g.zig_fmt_autosave = 0

			-- Mason install + auto-enable
			require("mason-lspconfig").setup({
				ensure_installed = {
					"lua_ls",
					"rust_analyzer",
					"gopls",
					"clangd",
					"texlab",
					"bashls",
					"arduino_language_server",
					"zls",
				},
				handlers = {
					-- Default handler: enable all servers except pylsp (using ty instead)
					function(server_name)
						if server_name ~= "pylsp" then
							vim.lsp.enable(server_name)
						end
					end,
				},
			})

			-- Enable ty manually (not available via Mason)
			vim.lsp.enable("ty")

			-- Tools used by conform
			require("mason-tool-installer").setup({
				ensure_installed = {
					"stylua",
					"black",
					"isort",
					"shfmt",
					"shellcheck",
					"gofumpt",
					"goimports",
					"prettierd",
					"codespell",
				},
			})
		end,
	},

	---------------------------------------------------------------------
	-- Completion
	---------------------------------------------------------------------
	{
		"saghen/blink.cmp",
		lazy = false,
		version = "1.*",
		dependencies = {
			{
				"L3MON4D3/LuaSnip",
				version = "2.*",
				build = (function()
					if vim.fn.has("win32") == 1 or vim.fn.executable("make") == 0 then
						return
					end
					return "make install_jsregexp"
				end)(),
				opts = {},
			},
			"folke/lazydev.nvim",
		},
		opts = {
			keymap = { preset = "default" },
			appearance = { nerd_font_variant = "mono" },
			completion = { documentation = { auto_show = false, auto_show_delay_ms = 500 } },
			sources = {
				default = { "lsp", "path", "snippets", "lazydev" },
				per_filetype = {
					minibuffer_input = {},
				},
				providers = { lazydev = { module = "lazydev.integrations.blink", score_offset = 100 } },
			},
			snippets = { preset = "luasnip" },
			fuzzy = { implementation = "lua" },
			signature = { enabled = true },
		},
	},

	---------------------------------------------------------------------
	-- Copilot
	---------------------------------------------------------------------
	{
		"zbirenbaum/copilot.lua",
		cmd = "Copilot",
		event = "InsertEnter",
		config = function()
			require("copilot").setup({
				suggestion = { enable = true, auto_trigger = true, keymap = { accept = "<C-l>" } },
			})
		end,
	},

	---------------------------------------------------------------------
	-- which-key
	---------------------------------------------------------------------
	{ "folke/which-key.nvim", event = "VeryLazy", opts = { icons = { group = "" } } },

	---------------------------------------------------------------------
	-- Snacks (picker bottom: ivy) + FIX: disable snacks.statuscolumn
	---------------------------------------------------------------------
	{
		"folke/snacks.nvim",
		priority = 1000,
		lazy = false,
		opts = {
			bigfile = { enabled = true },
			dashboard = { enabled = true },
			image = { enabled = true },
			indent = { enabled = true },
			input = { enabled = true },
			lazygit = { enabled = true },
			notifier = { enabled = true },

			picker = {
				enabled = true,
				layout = { preset = "ivy" },
			},

			-- IMPORTANT: let Neovim handle the number column normally
			-- (snacks.statuscolumn can override the number column)  [oai_citation:2‡Neovim](https://neovim.io/doc/user/news-0.11.html?utm_source=chatgpt.com)
			statuscolumn = { enabled = false },

			quickfile = { enabled = true },
			rename = { enabled = true },
			scope = { enabled = true },
			scroll = { enabled = true },
			words = { enabled = true },
		},
		keys = {
			{
				"<leader>G",
				function()
					Snacks.lazygit.open()
				end,
				desc = "LazyGit",
			},
			{
				"<leader>h",
				function()
					require("minibuffer").git_diff()
				end,
				desc = "Git diff files",
			},

			{ "<leader>u", vim.cmd.UndotreeToggle, desc = "Undo" },

			{
				"<leader>f",
				function()
					require("minibuffer").files()
				end,
				desc = "Files (Minibuffer)",
			},
			{
				"<leader>b",
				function()
					require("minibuffer").buffers()
				end,
				desc = "Buffers",
			},
			{
				"<leader>H",
				function()
					require("minibuffer").man()
				end,
				desc = "Man Pages",
			},
			{
				"<leader>g",
				function()
					require("minibuffer").grep()
				end,
				desc = "Grep",
			},

			-- keep <leader>d* for DAP
			{
				"<leader>sd",
				function()
					require("minibuffer").diagnostics()
				end,
				desc = "Search Diagnostics",
			},

			{
				"<leader><leader>",
				function()
					require("minibuffer").commands()
				end,
				desc = "Commands",
			},

			{
				"<leader>s",
				function()
					vim.cmd("vsplit")
				end,
				desc = "Split Vertically",
			},
			{
				"<leader>S",
				function()
					vim.cmd("split")
				end,
				desc = "Split Horizontally",
			},
			{
				"<leader>x",
				function()
					vim.cmd("only")
				end,
				desc = "Monocle",
			},

			{ "<leader>m", require("compmod").make_targets_async, desc = "Make" },
			{ "<leader>M", require("compmod").prompt_and_run_command_async, desc = "Compile" },
			{ "<leader>r", require("compmod").grep_async, desc = "Grep (custom)" },

			{ "<leader>w", "<cmd>w<cr>", desc = "Save Buffer" },
			{ "<leader>;", ":lua ", desc = "Eval" },
		},
	},

	---------------------------------------------------------------------
	-- Oil.nvim (file explorer)
	---------------------------------------------------------------------
	{
		"stevearc/oil.nvim",
		lazy = false,
		dependencies = { "nvim-tree/nvim-web-devicons" },
		keys = {
			{ "<leader>o", "<cmd>Oil<CR>", desc = "Oil" },
		},
		opts = {
			default_file_explorer = true,
			view_options = { show_hidden = true },
		},
	},

	---------------------------------------------------------------------
	-- DAP (all shortcuts under <leader>d...) + UI + adapters
	---------------------------------------------------------------------
	{
		"mfussenegger/nvim-dap",
		dependencies = {
			{ "rcarriga/nvim-dap-ui" },
			{ "nvim-neotest/nvim-nio" }, -- required  [oai_citation:3‡GitHub](https://github.com/nvim-neotest/nvim-nio?utm_source=chatgpt.com)
			{ "theHamsta/nvim-dap-virtual-text" },
			{ "mason-org/mason.nvim" },
			{ "jay-babu/mason-nvim-dap.nvim" },
		},
		keys = {
			{
				"<leader>dd",
				function()
					require("dap").continue()
				end,
				desc = "DAP Continue/Start",
			},
			{
				"<leader>dl",
				function()
					require("dap").run_last()
				end,
				desc = "DAP Run Last",
			},
			{
				"<leader>dt",
				function()
					require("dap").terminate()
				end,
				desc = "DAP Terminate",
			},
			{
				"<leader>dp",
				function()
					require("dap").pause()
				end,
				desc = "DAP Pause",
			},

			{
				"<leader>do",
				function()
					require("dap").step_over()
				end,
				desc = "DAP Step Over",
			},
			{
				"<leader>di",
				function()
					require("dap").step_into()
				end,
				desc = "DAP Step Into",
			},
			{
				"<leader>dO",
				function()
					require("dap").step_out()
				end,
				desc = "DAP Step Out",
			},

			{
				"<leader>db",
				function()
					require("dap").toggle_breakpoint()
				end,
				desc = "DAP Toggle Breakpoint",
			},
			{
				"<leader>dB",
				function()
					require("dap").set_breakpoint(vim.fn.input("Breakpoint condition: "))
				end,
				desc = "DAP Conditional Breakpoint",
			},
			{
				"<leader>dL",
				function()
					require("dap").set_breakpoint(nil, nil, vim.fn.input("Log point message: "))
				end,
				desc = "DAP Logpoint",
			},

			{
				"<leader>dr",
				function()
					require("dap").repl.toggle()
				end,
				desc = "DAP REPL Toggle",
			},
			{
				"<leader>du",
				function()
					require("dapui").toggle()
				end,
				desc = "DAP UI Toggle",
			},

			{
				"<leader>dh",
				function()
					require("dap.ui.widgets").hover()
				end,
				desc = "DAP Hover",
			},
			{
				"<leader>de",
				function()
					require("dapui").eval()
				end,
				desc = "DAP Eval",
			},
			{
				"<leader>de",
				function()
					require("dapui").eval()
				end,
				mode = "v",
				desc = "DAP Eval (visual)",
			},
		},
		config = function()
			local dap = require("dap")
			local dapui = require("dapui")

			dapui.setup()
			require("nvim-dap-virtual-text").setup()

			dap.listeners.before.attach.dapui_config = function()
				dapui.open()
			end
			dap.listeners.before.launch.dapui_config = function()
				dapui.open()
			end
			dap.listeners.before.event_terminated.dapui_config = function()
				dapui.close()
			end
			dap.listeners.before.event_exited.dapui_config = function()
				dapui.close()
			end

			vim.fn.sign_define("DapBreakpoint", { text = "●", texthl = "DiagnosticError" })
			vim.fn.sign_define("DapStopped", { text = "▶", texthl = "DiagnosticWarn", linehl = "Visual" })
			vim.fn.sign_define("DapBreakpointRejected", { text = "○", texthl = "DiagnosticHint" })

			require("mason-nvim-dap").setup({
				ensure_installed = {
					"python", -- debugpy
					"delve", -- go
					"codelldb", -- rust/c/cpp
					"cppdbg", -- c/cpp
				},
				handlers = {},
			})
		end,
	},

	---------------------------------------------------------------------
	-- Undo tree
	---------------------------------------------------------------------
	{ "mbbill/undotree" },

	---------------------------------------------------------------------
	-- Git signs
	---------------------------------------------------------------------
	{
		"lewis6991/gitsigns.nvim",
		opts = {
			signs = {
				add = { text = "+" },
				change = { text = "~" },
				delete = { text = "_" },
				topdelete = { text = "‾" },
				changedelete = { text = "~" },
			},
		},
	},

	---------------------------------------------------------------------
	-- Theme
	---------------------------------------------------------------------
	{
		"nyoom-engineering/oxocarbon.nvim",
		config = function()
			vim.opt.background = "dark"
			vim.cmd("colorscheme oxocarbon")
		end,
	},

	---------------------------------------------------------------------
	-- Comment.nvim (fixed <leader>c)
	---------------------------------------------------------------------
	{
		"numToStr/Comment.nvim",
		opts = {},
		keys = {
			{
				"<leader>c",
				function()
					require("Comment.api").toggle.linewise.current()
				end,
				desc = "Toggle comment",
			},
			{
				"<leader>c",
				function()
					local esc = vim.api.nvim_replace_termcodes("<Esc>", true, false, true)
					vim.api.nvim_feedkeys(esc, "nx", false)
					require("Comment.api").toggle.linewise(vim.fn.visualmode())
				end,
				mode = "v",
				desc = "Toggle comment",
			},
		},
	},

	---------------------------------------------------------------------
	-- Treesitter
	---------------------------------------------------------------------
	{
		"nvim-treesitter/nvim-treesitter",
		dependencies = {
			"nvim-treesitter/nvim-treesitter-textobjects",
			{ "nushell/tree-sitter-nu", build = ":TSUpdate nu" },
		},
		build = ":TSUpdate",
		config = function()
			require("nvim-treesitter.configs").setup({
				ensure_installed = {
					"vimdoc",
					"c",
					"cpp",
					"go",
					"lua",
					"python",
					"rust",
					"bash",
					"latex",
					"cmake",
					"ninja",
					"fennel",
					"arduino",
					"bibtex",
					"markdown",
					"markdown_inline",
					"commonlisp",
					"cuda",
					"json",
					"toml",
					"diff",
					"git_config",
					"git_rebase",
					"gitattributes",
					"gitcommit",
					"gitignore",
					"scheme",
					"ssh_config",
					"tmux",
					"nu",
				},
				sync_install = false,
				auto_install = true,
				highlight = { enable = true, additional_vim_regex_highlighting = false },
				indent = { enable = true },
			})
		end,
	},
	{ "nvim-treesitter/nvim-treesitter-context" },

	---------------------------------------------------------------------
	-- Format on save
	---------------------------------------------------------------------
	{
		"stevearc/conform.nvim",
		config = function()
			require("conform").setup({
				formatters_by_ft = {
					lua = { "stylua" },
					python = { "isort", "black" },
					bash = { "shfmt" },
					go = { "goimports", "gofumpt" },
					markdown = { "prettierd" },
					["*"] = { "codespell" },
					["_"] = { "trim_whitespace" },
				},
				format_on_save = { timeout_ms = 500, lsp_fallback = true },
			})
		end,
	},

	{ "folke/todo-comments.nvim", opts = {} },

	{
		"RRethy/vim-illuminate",
		opts = { delay = 200, large_file_cutoff = 2000 },
		config = function(_, opts)
			require("illuminate").configure(opts)
		end,
	},
}, {})

-----------------------------------------------------------------------
-- Options
-----------------------------------------------------------------------
vim.o.hlsearch = false
vim.o.incsearch = true

-- FIX: line numbers as global defaults (works for all windows)  [oai_citation:4‡Neovim](https://neovim.io/doc/user/options.html?utm_source=chatgpt.com)
vim.opt.number = true
vim.opt.relativenumber = true

vim.o.scrolloff = 8

vim.o.mouse = "a"
vim.o.clipboard = "unnamedplus"
vim.o.breakindent = true

vim.o.swapfile = false
vim.o.backup = false
vim.o.undofile = true
vim.o.undodir = os.getenv("HOME") .. "/.vim/undodir"
if vim.fn.isdirectory(vim.o.undodir) == 0 then
	vim.fn.mkdir(vim.o.undodir, "p")
end

vim.o.ignorecase = true
vim.o.smartcase = true
vim.wo.signcolumn = "yes"

vim.o.updatetime = 250
vim.o.timeoutlen = 300
vim.o.completeopt = "menu,menuone,noselect"
vim.o.termguicolors = true
vim.o.cmdheight = 0

vim.o.copyindent = true
vim.o.cursorline = true
vim.o.shiftwidth = 2
vim.o.tabstop = 2
vim.o.expandtab = true

vim.o.foldenable = true
vim.o.foldlevel = 99
vim.o.foldlevelstart = 99
vim.o.foldcolumn = vim.fn.has("nvim-0.9") == 1 and "1" or nil

-- Native statusline tuned to mimic the previous lualine sections:
-- A(mode) | B(git/diff/diag) | C(filename) ... X(encoding/fileformat/filetype) | Y(progress) | Z(location)
require("statusline").setup()

vim.o.history = 100
vim.o.laststatus = 3
vim.o.linebreak = true
vim.o.preserveindent = true
vim.o.pumheight = 10
vim.o.showmode = false
vim.o.splitbelow = true
vim.o.splitright = true
vim.o.title = true
vim.o.virtualedit = "block"
vim.o.wrap = false

require("avy").setup({
	timeout_ms = 1200,
})

-----------------------------------------------------------------------
-- Mappings (cleanup: no duplicates / no dead plugin refs)
-----------------------------------------------------------------------
vim.keymap.set({ "n", "v" }, "<Space>", "<Nop>", { silent = true })

-- Your "select word" remaps (kept)
vim.keymap.set("n", "w", "viw")
vim.keymap.set("n", "W", "viW")

-- Highlight on yank
vim.api.nvim_create_autocmd("TextYankPost", {
	group = vim.api.nvim_create_augroup("YankHighlight", { clear = true }),
	callback = function()
		vim.highlight.on_yank()
	end,
	pattern = "*",
})

-- Move selection up and down
vim.keymap.set("v", "J", ":m '>+1<CR>gv=gv", { desc = "Move selection down" })
vim.keymap.set("v", "K", ":m '<-2<CR>gv=gv", { desc = "Move selection up" })

-- Center search results
vim.keymap.set("n", "n", "nzzzv", { desc = "Next search (center)" })
vim.keymap.set("n", "N", "Nzzzv", { desc = "Prev search (center)" })

vim.keymap.set("i", "<C-c>", "<Esc>")
vim.keymap.set("n", "Q", "<nop>")

-- Resize splits
vim.keymap.set("n", "<C-Up>", "<cmd>resize -2<CR>", { desc = "Resize split up" })
vim.keymap.set("n", "<C-Down>", "<cmd>resize +2<CR>", { desc = "Resize split down" })
vim.keymap.set("n", "<C-Left>", "<cmd>vertical resize -2<CR>", { desc = "Resize split left" })
vim.keymap.set("n", "<C-Right>", "<cmd>vertical resize +2<CR>", { desc = "Resize split right" })

-- Deal with wrap
vim.keymap.set("n", "k", "v:count == 0 ? 'gk' : 'k'", { expr = true, silent = true })
vim.keymap.set("n", "j", "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })

-- Indentation
vim.keymap.set("v", "<S-Tab>", "<gv", { desc = "Unindent line" })
vim.keymap.set("v", "<Tab>", ">gv", { desc = "Indent line" })

-- Jump list
vim.keymap.set("n", "<S-Tab>", "<C-i>", { desc = "Jump to next position" })
vim.keymap.set("n", "<Tab>", "<C-o>", { desc = "Jump to previous position" })

-- Escape insert mode
vim.keymap.set("i", "jj", "<Esc>", { desc = "Escape insert mode" })
vim.keymap.set("i", "jk", "<Esc>", { desc = "Escape insert mode" })

-- Insert mode: Emacs/Readline-style editing
vim.keymap.set("i", "<C-a>", "<C-o>0", { desc = "Line start (readline)" })
vim.keymap.set("i", "<C-e>", "<End>", { desc = "Line end (readline)" })
vim.keymap.set("i", "<C-b>", "<Left>", { desc = "Backward char (readline)" })
vim.keymap.set("i", "<C-f>", "<Right>", { desc = "Forward char (readline)" })
vim.keymap.set("i", "<M-b>", "<C-o>b", { desc = "Backward word (readline)" })
vim.keymap.set("i", "<M-f>", "<C-o>w", { desc = "Forward word (readline)" })
vim.keymap.set("i", "<C-d>", "<Del>", { desc = "Delete char (readline)" })
vim.keymap.set("i", "<C-h>", "<BS>", { desc = "Backward delete char (readline)" })
vim.keymap.set("i", "<C-k>", "<C-o>D", { desc = "Kill to end of line (readline)" })
vim.keymap.set("i", "<C-u>", "<C-o>d0", { desc = "Kill to start of line (readline)" })
vim.keymap.set("i", "<M-d>", "<C-o>de", { desc = "Kill word forward (readline)" })
vim.keymap.set("i", "<M-BS>", "<C-w>", { desc = "Kill word backward (readline)" })
vim.keymap.set("i", "<C-y>", '<C-r>"', { desc = "Yank text (readline)" })

-- Git hunks
vim.keymap.set("n", "ç", function()
	require("gitsigns").nav_hunk("next")
end, { desc = "Next hunk" })
vim.keymap.set("n", "Ç", function()
	require("gitsigns").nav_hunk("prev")
end, { desc = "Prev hunk" })

-- Tmux-aware pane movement
vim.keymap.set("n", "<C-h>", tmux_move_left, { noremap = true, desc = "Left" })
vim.keymap.set("n", "<C-j>", tmux_move_down, { noremap = true, desc = "Down" })
vim.keymap.set("n", "<C-k>", tmux_move_up, { noremap = true, desc = "Up" })
vim.keymap.set("n", "<C-l>", tmux_move_right, { noremap = true, desc = "Right" })

-- Terminal
vim.keymap.set("n", "<leader>t", function()
	Snacks.terminal()
end, { desc = "Terminal" })

-- Close window
vim.keymap.set("n", "<C-q>", "<cmd>clo<cr>", { desc = "Close Window" })

-- File picker in insert mode
vim.keymap.set("i", "<C-x><C-f>", function()
	ido_open_or_fallback({ source = "files", prompt = "Find file: ", include_root = true }, function()
		Snacks.picker.files()
	end)
end, { silent = true, desc = "Pick file" })
