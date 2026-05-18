-- Single-file Neovim config.

vim.g.mapleader = " "
vim.g.maplocalleader = ","
vim.g.have_nerd_font = true

local fn = vim.fn
local api = vim.api

local function map(mode, lhs, rhs, desc, opts)
	opts = vim.tbl_extend("force", { silent = true, desc = desc }, opts or {})
	vim.keymap.set(mode, lhs, rhs, opts)
end

local function executable(cmd)
	return type(cmd) == "string" and fn.executable(cmd) == 1
end

local mason_bin = fn.stdpath("data") .. "/mason/bin"
if fn.isdirectory(mason_bin) == 1 then
	vim.env.PATH = mason_bin .. ":" .. vim.env.PATH
end

vim.opt.packpath:prepend(fn.stdpath("data") .. "/site")

local compmod_src = "https://github.com/JNSFilipe/compmod.nvim"
local compmod_local = fn.expand("~/Documents/GitHub/compmod.nvim")
if fn.isdirectory(compmod_local) == 1 then
	compmod_src = "file://" .. compmod_local
end

vim.pack.add({
	{ src = "https://github.com/folke/snacks.nvim" },
	{ src = "https://github.com/folke/which-key.nvim" },
	{ src = "https://github.com/mason-org/mason.nvim" },
	{ src = "https://github.com/nyoom-engineering/oxocarbon.nvim" },
	{ src = "https://github.com/lewis6991/gitsigns.nvim" },
	{ src = compmod_src, name = "compmod.nvim" },
	-- { src = "https://github.com/mfussenegger/nvim-dap" },
}, { confirm = false, load = true })

vim.opt.background = "dark"
pcall(vim.cmd.colorscheme, "oxocarbon")

require("snacks").setup({
	bigfile = {},
	explorer = { replace_netrw = true },
	indent = {},
	input = {},
	lazygit = {},
	notifier = {},
	picker = {
		layout = { preset = "ivy" },
		sources = {
			explorer = { hidden = true, ignored = true },
			files = { hidden = true, ignored = false },
		},
	},
	quickfile = {},
	rename = {},
	scope = {},
	scroll = {},
	statuscolumn = { enabled = false },
	words = {},
})

local ok_which_key, which_key = pcall(require, "which-key")
if ok_which_key then
	which_key.setup({
		icons = { group = "" },
	})
	which_key.add({
		{ "gr", group = "LSP" },
		{ "<leader>s", group = "Search / splits" },
	})
end

local ok_compmod, compmod = pcall(require, "compmod")

local function picker(name, opts)
	return function()
		Snacks.picker[name](opts or {})
	end
end

local function notify_missing(feature, hint)
	vim.notify(feature .. " is not available" .. (hint and (": " .. hint) or ""), vim.log.levels.WARN)
end

------------------------------------------------------------------------
-- Options
------------------------------------------------------------------------
vim.opt.hlsearch = false
vim.opt.incsearch = true
vim.opt.number = true
vim.opt.relativenumber = true
vim.opt.scrolloff = 8
vim.opt.mouse = "a"
vim.opt.clipboard = "unnamedplus"
vim.opt.breakindent = true
vim.opt.swapfile = false
vim.opt.backup = false
vim.opt.undofile = true
vim.opt.undodir = vim.env.HOME .. "/.vim/undodir"
vim.opt.ignorecase = true
vim.opt.smartcase = true
vim.opt.signcolumn = "yes"
vim.opt.updatetime = 250
vim.opt.timeoutlen = 300
vim.opt.completeopt = { "menu", "menuone", "noselect", "popup" }
vim.opt.termguicolors = true
vim.opt.cmdheight = 0
vim.opt.copyindent = true
vim.opt.cursorline = true
vim.opt.shiftwidth = 2
vim.opt.tabstop = 2
vim.opt.expandtab = true
vim.opt.foldenable = true
vim.opt.foldlevel = 99
vim.opt.foldlevelstart = 99
vim.opt.foldcolumn = "1"
vim.opt.history = 100
vim.opt.laststatus = 3
vim.opt.linebreak = true
vim.opt.preserveindent = true
vim.opt.pumheight = 10
vim.opt.showmode = false
vim.opt.splitbelow = true
vim.opt.splitright = true
vim.opt.title = true
vim.opt.virtualedit = "block"
vim.opt.wrap = false
vim.opt.grepprg = "rg --vimgrep --smart-case"
vim.opt.grepformat = "%f:%l:%c:%m"

if fn.isdirectory(vim.o.undodir) == 0 then
	fn.mkdir(vim.o.undodir, "p")
end

function _G.snacks_alt_statusline()
	local mode = api.nvim_get_mode().mode:upper()
	local branch = vim.b.gitsigns_head
	local filename = fn.expand("%:t")
	if filename == "" then
		filename = "[No Name]"
	end
	local modified = vim.bo.modified and " +" or ""
	local readonly = vim.bo.readonly and " RO" or ""
	local diagnostics = ""

	for severity, label in pairs({
		[vim.diagnostic.severity.ERROR] = "E",
		[vim.diagnostic.severity.WARN] = "W",
		[vim.diagnostic.severity.INFO] = "I",
		[vim.diagnostic.severity.HINT] = "H",
	}) do
		local count = #vim.diagnostic.get(0, { severity = severity })
		if count > 0 then
			diagnostics = diagnostics .. (" %s:%d"):format(label, count)
		end
	end

	return table.concat({
		" ",
		mode,
		branch and ("  " .. branch) or "",
		diagnostics,
		"  ",
		filename,
		modified,
		readonly,
		"%=",
		" ",
		vim.bo.fileencoding ~= "" and vim.bo.fileencoding or vim.o.encoding,
		" ",
		vim.bo.fileformat,
		" ",
		vim.bo.filetype,
		"  %p%% %l:%c ",
	})
end

vim.o.statusline = "%!v:lua.snacks_alt_statusline()"

------------------------------------------------------------------------
-- Built-in LSP and completion
------------------------------------------------------------------------
local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities.textDocument.completion.completionItem.snippetSupport = true
vim.lsp.config("*", { capabilities = capabilities })

vim.diagnostic.config({
	severity_sort = true,
	float = { border = "rounded", source = "if_many" },
	underline = { severity = vim.diagnostic.severity.ERROR },
	signs = vim.g.have_nerd_font and {
		text = {
			[vim.diagnostic.severity.ERROR] = "E",
			[vim.diagnostic.severity.WARN] = "W",
			[vim.diagnostic.severity.INFO] = "I",
			[vim.diagnostic.severity.HINT] = "H",
		},
	} or {},
	virtual_text = { source = "if_many", spacing = 2 },
})

api.nvim_create_autocmd("LspAttach", {
	group = api.nvim_create_augroup("snacks-alt-lsp", { clear = true }),
	callback = function(event)
		local bufnr = event.buf
		local client = vim.lsp.get_client_by_id(event.data.client_id)
		if not client then
			return
		end

		local function lsp_map(mode, lhs, rhs, desc)
			map(mode, lhs, rhs, "LSP: " .. desc, { buffer = bufnr })
		end

		lsp_map("n", "grn", vim.lsp.buf.rename, "[R]e[n]ame")
		lsp_map({ "n", "x" }, "gra", vim.lsp.buf.code_action, "Code [A]ction")
		lsp_map("n", "gd", vim.lsp.buf.definition, "Go to [D]efinition")
		lsp_map("n", "grr", picker("lsp_references"), "[R]eferences")
		lsp_map("n", "gri", picker("lsp_implementations"), "[I]mplementation")
		lsp_map("n", "grd", picker("lsp_definitions"), "[D]efinition")
		lsp_map("n", "grD", vim.lsp.buf.declaration, "[D]eclaration")
		lsp_map("n", "grt", picker("lsp_type_definitions"), "[T]ype Definition")
		lsp_map("n", "gO", picker("lsp_symbols"), "Document Symbols")
		lsp_map("n", "gW", picker("lsp_workspace_symbols"), "Workspace Symbols")
		lsp_map("i", "<C-x><C-k>", vim.lsp.buf.signature_help, "Signature Help")

		if client:supports_method("textDocument/completion") then
			vim.lsp.completion.enable(true, client.id, bufnr, { autotrigger = true })
		end

		if client:supports_method("textDocument/documentHighlight") then
			local highlight_group = api.nvim_create_augroup("snacks-alt-lsp-highlight", { clear = false })
			api.nvim_create_autocmd({ "CursorHold", "CursorHoldI" }, {
				group = highlight_group,
				buffer = bufnr,
				callback = vim.lsp.buf.document_highlight,
			})
			api.nvim_create_autocmd({ "CursorMoved", "CursorMovedI" }, {
				group = highlight_group,
				buffer = bufnr,
				callback = vim.lsp.buf.clear_references,
			})
		end

		if client:supports_method("textDocument/inlayHint") then
			lsp_map("n", "<leader>th", function()
				local enabled = vim.lsp.inlay_hint.is_enabled({ bufnr = bufnr })
				vim.lsp.inlay_hint.enable(not enabled, { bufnr = bufnr })
			end, "[T]oggle Inlay [H]ints")
		end

		if client:supports_method("textDocument/formatting") then
			api.nvim_create_autocmd("BufWritePre", {
				group = api.nvim_create_augroup("snacks-alt-lsp-format", { clear = false }),
				buffer = bufnr,
				callback = function()
					vim.lsp.buf.format({ bufnr = bufnr, id = client.id, timeout_ms = 1000 })
				end,
			})
		end
	end,
})

local servers = {
	lua_ls = {
		mason = "lua-language-server",
		cmd = { "lua-language-server" },
		filetypes = { "lua" },
		root_markers = { ".luarc.json", ".luarc.jsonc", ".luacheckrc", ".stylua.toml", "stylua.toml", ".git" },
		settings = {
			Lua = {
				completion = { callSnippet = "Replace" },
				diagnostics = { globals = { "vim", "Snacks", "it", "describe", "before_each", "after_each" } },
			},
		},
	},
	clangd = {
		mason = "clangd",
		cmd = {
			"clangd",
			"--background-index",
			"--clang-tidy",
			"--header-insertion=iwyu",
			"--completion-style=detailed",
			"--function-arg-placeholders",
			"--fallback-style=llvm",
		},
		filetypes = { "c", "cpp", "objc", "objcpp", "cuda" },
		root_markers = { "compile_commands.json", "compile_flags.txt", "Makefile", ".git" },
		init_options = { usePlaceholders = true, completeUnimported = true, clangdFileStatus = true },
	},
	ty = {
		mason = "ty",
		cmd = { "ty", "server" },
		filetypes = { "python" },
		root_markers = { "pyproject.toml", "setup.py", "setup.cfg", "requirements.txt", ".git" },
	},
	zls = {
		mason = "zls",
		cmd = { "zls" },
		filetypes = { "zig", "zir" },
		root_markers = { "build.zig", "zls.json", ".git" },
		settings = { zls = { enable_inlay_hints = true, enable_snippets = true, warn_style = true } },
	},
	rust_analyzer = {
		mason = "rust-analyzer",
		cmd = { "rust-analyzer" },
		filetypes = { "rust" },
		root_markers = { "Cargo.toml", "rust-project.json", ".git" },
	},
	gopls = {
		mason = "gopls",
		cmd = { "gopls" },
		filetypes = { "go", "gomod", "gowork", "gotmpl" },
		root_markers = { "go.work", "go.mod", ".git" },
	},
	texlab = {
		mason = "texlab",
		cmd = { "texlab" },
		filetypes = { "tex", "plaintex", "bib" },
		root_markers = { ".latexmkrc", "latexmkrc", "main.tex", ".git" },
	},
	bashls = {
		mason = "bash-language-server",
		cmd = { "bash-language-server", "start" },
		filetypes = { "sh", "bash", "zsh" },
		root_markers = { ".git" },
	},
	arduino_language_server = {
		mason = "arduino-language-server",
		cmd = { "arduino-language-server" },
		filetypes = { "arduino" },
		root_markers = { "sketch.yaml", ".git" },
	},
}

local function configure_lsp_server(name, server)
	local config = vim.deepcopy(server)
	config.mason = nil
	vim.lsp.config(name, config)
end

local function enable_lsp_server(name, server)
	configure_lsp_server(name, server)
	if executable(server.cmd and server.cmd[1]) then
		vim.lsp.enable(name)
		return true
	end
	return false
end

for name, server in pairs(servers) do
	enable_lsp_server(name, server)
end

local ok_mason, mason = pcall(require, "mason")
if ok_mason then
	mason.setup()
end

local function ensure_mason_lsp_servers()
	if not ok_mason or #api.nvim_list_uis() == 0 then
		return
	end

	local ok_registry, registry = pcall(require, "mason-registry")
	if not ok_registry then
		return
	end

	registry.refresh(function()
		for name, server in pairs(servers) do
			if server.mason and not executable(server.cmd and server.cmd[1]) then
				local ok_package, package = pcall(registry.get_package, server.mason)
				if ok_package and not package:is_installed() and not package:is_installing() then
					package:install({}, function(success)
						if success then
							vim.schedule(function()
								enable_lsp_server(name, server)
							end)
						end
					end)
				elseif ok_package and package:is_installed() then
					vim.schedule(function()
						enable_lsp_server(name, server)
					end)
				elseif not ok_package then
					vim.schedule(function()
						vim.notify("Mason package unavailable: " .. server.mason, vim.log.levels.WARN)
					end)
				end
			end
		end
	end)
end

ensure_mason_lsp_servers()

vim.g.zig_fmt_parse_errors = 0
vim.g.zig_fmt_autosave = 0

------------------------------------------------------------------------
-- Git plugin setup
------------------------------------------------------------------------
local ok_gitsigns, gitsigns = pcall(require, "gitsigns")
if ok_gitsigns then
	gitsigns.setup({
		signs = {
			add = { text = "+" },
			change = { text = "~" },
			delete = { text = "_" },
			topdelete = { text = "‾" },
			changedelete = { text = "~" },
		},
	})
end

-- DAP is intentionally disabled in this minimal Snacks-first config for now.
--[[
local ok_dap, dap = pcall(require, "dap")
if ok_dap then
	fn.sign_define("DapBreakpoint", { text = "●", texthl = "DiagnosticError" })
	fn.sign_define("DapStopped", { text = "▶", texthl = "DiagnosticWarn", linehl = "Visual" })
	fn.sign_define("DapBreakpointRejected", { text = "○", texthl = "DiagnosticHint" })
end
]]

------------------------------------------------------------------------
-- Commands and helpers
------------------------------------------------------------------------
local function in_tmux()
	local t = vim.env.TMUX
	return t ~= nil and t ~= ""
end

local function tmux_cmd(args)
	if in_tmux() and executable("tmux") then
		vim.cmd("silent !tmux " .. args)
	end
end

local function tmux_move(direction, command)
	if fn.winnr(direction) ~= fn.winnr() then
		vim.cmd("wincmd " .. direction)
	else
		tmux_cmd(command)
	end
end

------------------------------------------------------------------------
-- Mappings
------------------------------------------------------------------------
map({ "n", "v" }, "<Space>", "<Nop>", nil, { silent = true })

map("n", "w", "viw", "Select word")
map("n", "W", "viW", "Select WORD")

api.nvim_create_autocmd("TextYankPost", {
	group = api.nvim_create_augroup("snacks-alt-yank-highlight", { clear = true }),
	callback = function()
		vim.highlight.on_yank()
	end,
})

map("v", "J", ":m '>+1<CR>gv=gv", "Move selection down")
map("v", "K", ":m '<-2<CR>gv=gv", "Move selection up")
map("n", "n", "nzzzv", "Next search result")
map("n", "N", "Nzzzv", "Previous search result")
map("i", "<C-c>", "<Esc>", "Escape insert mode")
map("n", "Q", "<nop>")

map("n", "<C-Up>", "<cmd>resize -2<CR>", "Resize split up")
map("n", "<C-Down>", "<cmd>resize +2<CR>", "Resize split down")
map("n", "<C-Left>", "<cmd>vertical resize -2<CR>", "Resize split left")
map("n", "<C-Right>", "<cmd>vertical resize +2<CR>", "Resize split right")
map("n", "k", "v:count == 0 ? 'gk' : 'k'", nil, { expr = true, silent = true })
map("n", "j", "v:count == 0 ? 'gj' : 'j'", nil, { expr = true, silent = true })
map("v", "<S-Tab>", "<gv", "Unindent line")
map("v", "<Tab>", ">gv", "Indent line")
map("n", "<S-Tab>", "<C-i>", "Jump to next position")
map("n", "<Tab>", "<C-o>", "Jump to previous position")
map("i", "jj", "<Esc>", "Escape insert mode")
map("i", "jk", "<Esc>", "Escape insert mode")

map("i", "<C-a>", "<C-o>0", "Line start")
map("i", "<C-e>", "<End>", "Line end")
map("i", "<C-b>", "<Left>", "Backward char")
map("i", "<C-f>", "<Right>", "Forward char")
map("i", "<M-b>", "<C-o>b", "Backward word")
map("i", "<M-f>", "<C-o>w", "Forward word")
map("i", "<C-d>", "<Del>", "Delete char")
map("i", "<C-h>", "<BS>", "Backward delete char")
map("i", "<C-k>", "<C-o>D", "Kill to end of line")
map("i", "<C-u>", "<C-o>d0", "Kill to start of line")
map("i", "<M-d>", "<C-o>de", "Kill word forward")
map("i", "<M-BS>", "<C-w>", "Kill word backward")
map("i", "<C-y>", '<C-r>"', "Yank text")
map("i", "<C-Space>", vim.lsp.completion.get, "Trigger completion")
map("i", "<C-x><C-f>", picker("files"), "Pick file")

map("n", "ç", function()
	if ok_gitsigns then
		gitsigns.nav_hunk("next")
	else
		Snacks.picker.git_diff()
	end
end, "Next hunk")
map("n", "Ç", function()
	if ok_gitsigns then
		gitsigns.nav_hunk("prev")
	else
		Snacks.picker.git_diff()
	end
end, "Previous hunk")

map("n", "<C-h>", function()
	tmux_move("h", "select-pane -L")
end, "Left")
map("n", "<C-j>", function()
	tmux_move("j", "select-pane -D")
end, "Down")
map("n", "<C-k>", function()
	tmux_move("k", "select-pane -U")
end, "Up")
map("n", "<C-l>", function()
	tmux_move("l", "select-pane -R")
end, "Right")

map("n", "<leader>G", function()
	Snacks.lazygit.open()
end, "LazyGit")
map("n", "<leader>h", picker("git_diff"), "Git diff")
map("n", "<leader>u", picker("undo"), "Undo history")
map("n", "<leader>f", picker("files"), "Files")
map("n", "<leader>b", picker("buffers"), "Buffers")
map("n", "<leader>H", picker("man"), "Man pages")
map("n", "<leader>g", picker("grep"), "Grep")
map("n", "<leader>sd", picker("diagnostics"), "Search diagnostics")
map("n", "<leader><leader>", picker("commands"), "Commands")
map("n", "<leader>s", "<cmd>vsplit<CR>", "Split vertically")
map("n", "<leader>S", "<cmd>split<CR>", "Split horizontally")
map("n", "<leader>x", "<cmd>only<CR>", "Monocle")
map("n", "<leader>m", function()
	if ok_compmod then
		compmod.make_targets_async()
	else
		notify_missing("compmod.nvim", "package not loaded")
	end
end, "Make")
map("n", "<leader>M", function()
	if ok_compmod then
		compmod.prompt_and_run_command_async()
	else
		notify_missing("compmod.nvim", "package not loaded")
	end
end, "Compile")
map("n", "<leader>r", function()
	if ok_compmod then
		compmod.grep_async()
	else
		notify_missing("compmod.nvim", "package not loaded")
	end
end, "Grep")
map("n", "<leader>w", "<cmd>write<CR>", "Save buffer")
map("n", "<leader>;", ":lua ", "Eval", { silent = false })
map("n", "<leader>o", function()
	Snacks.explorer()
end, "Explorer")
map("n", "<leader>t", function()
	Snacks.terminal()
end, "Terminal")
map("n", "<C-q>", "<cmd>close<CR>", "Close window")

map("n", "<leader>c", "gcc", "Toggle comment", { remap = true })
map("x", "<leader>c", "gc", "Toggle comment", { remap = true })

-- DAP mappings are intentionally disabled with the DAP setup above.
--[[
if ok_dap then
	map("n", "<leader>dd", dap.continue, "DAP Continue/Start")
	map("n", "<leader>dl", dap.run_last, "DAP Run Last")
	map("n", "<leader>dt", dap.terminate, "DAP Terminate")
	map("n", "<leader>dp", dap.pause, "DAP Pause")
	map("n", "<leader>do", dap.step_over, "DAP Step Over")
	map("n", "<leader>di", dap.step_into, "DAP Step Into")
	map("n", "<leader>dO", dap.step_out, "DAP Step Out")
	map("n", "<leader>db", dap.toggle_breakpoint, "DAP Toggle Breakpoint")
	map("n", "<leader>dB", function()
		dap.set_breakpoint(vim.fn.input("Breakpoint condition: "))
	end, "DAP Conditional Breakpoint")
	map("n", "<leader>dL", function()
		dap.set_breakpoint(nil, nil, vim.fn.input("Log point message: "))
	end, "DAP Logpoint")
	map("n", "<leader>dr", dap.repl.toggle, "DAP REPL Toggle")
	map("n", "<leader>du", function()
		local widgets = require("dap.ui.widgets")
		widgets.sidebar(widgets.scopes).toggle()
	end, "DAP Scopes")
	map("n", "<leader>dh", function()
		require("dap.ui.widgets").hover()
	end, "DAP Hover")
	map({ "n", "x" }, "<leader>de", function()
		require("dap.ui.widgets").eval()
	end, "DAP Eval")
else
	for _, lhs in ipairs({ "dd", "dl", "dt", "dp", "do", "di", "dO", "db", "dB", "dL", "dr", "du", "dh", "de" }) do
		map("n", "<leader>" .. lhs, function()
			notify_missing("DAP", "nvim-dap did not load")
		end, "DAP unavailable")
	end
end
]]
