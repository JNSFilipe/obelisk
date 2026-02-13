local M = {}

local function hl(name)
	local ok, value = pcall(vim.api.nvim_get_hl, 0, { name = name, link = false })
	if ok and type(value) == "table" then
		return value
	end
	return {}
end

local function pick(names, key)
	for _, name in ipairs(names) do
		local value = hl(name)[key]
		if value then
			return value
		end
	end
	return nil
end

local function set_statusline_highlights()
	local base_bg = pick({ "StatusLine", "Normal" }, "bg")
	local base_fg = pick({ "StatusLine", "Normal" }, "fg")
	local muted_bg = pick({ "StatusLineNC", "Folded", "Pmenu", "NormalFloat", "Normal" }, "bg")
	local sep_fg = pick({ "Comment", "WinSeparator", "StatusLineNC", "Normal" }, "fg")

	local accent_normal = pick({ "Function", "Directory", "Identifier", "Statement" }, "fg")
	local accent_insert = pick({ "String", "DiffAdd", "DiagnosticOk", "Identifier" }, "fg")
	local accent_visual = pick({ "Constant", "Type", "DiffChange", "Identifier" }, "fg")
	local accent_replace = pick({ "ErrorMsg", "DiagnosticError", "DiffDelete", "Identifier" }, "fg")
	local accent_command = pick({ "Statement", "Keyword", "Identifier" }, "fg")
	local accent_terminal = pick({ "Special", "Type", "Identifier" }, "fg")
	local accent_y = pick({ "Type", "Identifier", "Function" }, "fg")
	local accent_z = pick({ "Special", "Function", "Identifier" }, "fg")

	vim.api.nvim_set_hl(0, "ObeliskStSep", { fg = sep_fg, bg = base_bg })
	vim.api.nvim_set_hl(0, "ObeliskStA", { fg = base_bg, bg = accent_normal, bold = true })
	vim.api.nvim_set_hl(0, "ObeliskStB", { fg = base_fg, bg = base_bg })
	vim.api.nvim_set_hl(0, "ObeliskStC", { fg = base_fg, bg = base_bg })
	vim.api.nvim_set_hl(0, "ObeliskStX", { fg = base_fg, bg = muted_bg })
	vim.api.nvim_set_hl(0, "ObeliskStY", { fg = base_bg, bg = accent_y, bold = true })
	vim.api.nvim_set_hl(0, "ObeliskStZ", { fg = base_bg, bg = accent_z, bold = true })

	vim.api.nvim_set_hl(0, "ObeliskStModeNormal", { fg = base_bg, bg = accent_normal, bold = true })
	vim.api.nvim_set_hl(0, "ObeliskStModeInsert", { fg = base_bg, bg = accent_insert, bold = true })
	vim.api.nvim_set_hl(0, "ObeliskStModeVisual", { fg = base_bg, bg = accent_visual, bold = true })
	vim.api.nvim_set_hl(0, "ObeliskStModeReplace", { fg = base_bg, bg = accent_replace, bold = true })
	vim.api.nvim_set_hl(0, "ObeliskStModeCommand", { fg = base_bg, bg = accent_command, bold = true })
	vim.api.nvim_set_hl(0, "ObeliskStModeTerminal", { fg = base_bg, bg = accent_terminal, bold = true })
end

local function status_mode()
	local mode = vim.api.nvim_get_mode().mode
	local map = {
		n = "NORMAL",
		no = "N-PENDING",
		i = "INSERT",
		ic = "INSERT",
		v = "VISUAL",
		V = "V-LINE",
		["\22"] = "V-BLOCK",
		c = "COMMAND",
		R = "REPLACE",
		Rv = "V-REPLACE",
		s = "SELECT",
		S = "S-LINE",
		["\19"] = "S-BLOCK",
		t = "TERMINAL",
	}
	return map[mode] or mode
end

local function status_mode_hl()
	local mode = vim.api.nvim_get_mode().mode
	local m = mode:sub(1, 1)
	if m == "i" then
		return "ObeliskStModeInsert"
	end
	if m == "v" or m == "V" or mode == "\22" then
		return "ObeliskStModeVisual"
	end
	if m == "R" then
		return "ObeliskStModeReplace"
	end
	if m == "c" then
		return "ObeliskStModeCommand"
	end
	if m == "t" then
		return "ObeliskStModeTerminal"
	end
	return "ObeliskStModeNormal"
end

local branch_cache = {}

local function fallback_branch()
	local file_dir = vim.fn.expand("%:p:h")
	if file_dir == "" then
		file_dir = (vim.uv or vim.loop).cwd() or "."
	end

	local now = (vim.uv or vim.loop).now()
	local cached = branch_cache[file_dir]
	if cached and (now - cached.at) < 3000 then
		return cached.value
	end

	local out = vim.system({ "git", "-C", file_dir, "rev-parse", "--abbrev-ref", "HEAD" }, { text = true }):wait()
	local value = ""
	if out.code == 0 then
		value = (out.stdout or ""):gsub("%s+$", "")
		if value == "HEAD" then
			value = ""
		end
	end

	branch_cache[file_dir] = { value = value, at = now }
	return value
end

local function status_git()
	local d = vim.b.gitsigns_status_dict
	local head = (type(d) == "table" and d.head) or vim.b.gitsigns_head
	if not head or head == "" then
		head = fallback_branch()
	end

	local parts = {}
	if head and head ~= "" then
		local icon = vim.g.have_nerd_font and " " or "git:"
		table.insert(parts, icon .. head)
		if type(d) == "table" then
			if (d.added or 0) > 0 then
				table.insert(parts, "+" .. d.added)
			end
			if (d.changed or 0) > 0 then
				table.insert(parts, "~" .. d.changed)
			end
			if (d.removed or 0) > 0 then
				table.insert(parts, "-" .. d.removed)
			end
		end
	end
	return table.concat(parts, " ")
end

local function status_diagnostics()
	local sev = vim.diagnostic.severity
	local e = #vim.diagnostic.get(0, { severity = sev.ERROR })
	local w = #vim.diagnostic.get(0, { severity = sev.WARN })
	local h = #vim.diagnostic.get(0, { severity = sev.HINT })
	local i = #vim.diagnostic.get(0, { severity = sev.INFO })

	local parts = {}
	if e > 0 then
		table.insert(parts, "E" .. e)
	end
	if w > 0 then
		table.insert(parts, "W" .. w)
	end
	if h > 0 then
		table.insert(parts, "H" .. h)
	end
	if i > 0 then
		table.insert(parts, "I" .. i)
	end

	return table.concat(parts, " ")
end

local function status_b_section()
	local git = status_git()
	local diag = status_diagnostics()
	local parts = {}
	if git ~= "" then
		table.insert(parts, git)
	end
	if diag ~= "" then
		table.insert(parts, diag)
	end
	return table.concat(parts, " | ")
end

local function status_x_parts()
	local enc = vim.bo.fileencoding ~= "" and vim.bo.fileencoding or vim.o.encoding
	return enc, vim.bo.fileformat
end

function M.render()
	local sec_b = status_b_section()
	local enc, fileformat = status_x_parts()
	local chunks = {
		"%#",
		status_mode_hl(),
		"# ",
		status_mode(),
		" ",
		"%#ObeliskStSep#|",
	}

	if sec_b ~= "" then
		table.insert(chunks, "%#ObeliskStB# " .. sec_b .. " ")
		table.insert(chunks, "%#ObeliskStSep#|")
	end

	table.insert(chunks, "%#ObeliskStC# %<%f%m%r%h%w ")
	table.insert(chunks, "%=")
	table.insert(chunks, "%#ObeliskStC# " .. enc .. " ")
	table.insert(chunks, "%#ObeliskStSep#|")
	table.insert(chunks, "%#ObeliskStC# " .. fileformat .. " ")
	table.insert(chunks, "%#ObeliskStSep#|")
	table.insert(chunks, "%#ObeliskStC# %y ")
	table.insert(chunks, "%#ObeliskStSep#|")
	table.insert(chunks, "%#ObeliskStY# %p%% ")
	table.insert(chunks, "%#ObeliskStSep#|")
	table.insert(chunks, "%#ObeliskStZ# %l:%c ")
	table.insert(chunks, "%*")

	return table.concat(chunks)
end

function M.setup()
	set_statusline_highlights()
	vim.api.nvim_create_autocmd("ColorScheme", {
		group = vim.api.nvim_create_augroup("ObeliskStatuslineHL", { clear = true }),
		callback = set_statusline_highlights,
	})
	vim.o.statusline = "%!v:lua.require'statusline'.render()"
end

return M
