local api = vim.api

local M = {}

local active_ui = nil
local hl_setup = false

local schemas = {
    buffer = {
        pattern = "^(%d+):%s+(.-):(%d+):(%d+)",
        keys = { "bufnr", "filename", "lnum", "col" },
        types = { bufnr = tonumber, lnum = tonumber, col = tonumber },
    },
    lsp = {
        pattern = "^(.-):(%d+):(%d+)",
        keys = { "filename", "lnum", "col" },
        types = { lnum = tonumber, col = tonumber },
    },
    grep = {
        pattern = "^(.-):(%d+):(%d+):(.*)",
        keys = { "filename", "lnum", "col", "content" },
        types = { lnum = tonumber, col = tonumber },
    },
    file = {
        pattern = "^(.*)",
        keys = { "filename" },
        types = {},
    },
}

local function setup_highlights()
    if hl_setup then
        return
    end
    hl_setup = true

    vim.api.nvim_set_hl(0, "MinibufferNormal", { link = "Normal" })
    vim.api.nvim_set_hl(0, "MinibufferPrompt", { link = "Title" })
    vim.api.nvim_set_hl(0, "MinibufferSelected", { link = "Visual" })
    vim.api.nvim_set_hl(0, "MinibufferMatch", { link = "Identifier" })
    vim.api.nvim_set_hl(0, "MinibufferMatchChar", { link = "Search" })
    vim.api.nvim_set_hl(0, "MinibufferNoMatch", { link = "WarningMsg" })
    vim.api.nvim_set_hl(0, "MinibufferSeparator", { link = "Comment" })
    vim.api.nvim_set_hl(0, "MinibufferMark", { link = "String" })
end

local function close_existing()
    if active_ui then
        active_ui:close()
        active_ui = nil
    end

    for _, win in ipairs(api.nvim_list_wins()) do
        if api.nvim_win_is_valid(win) then
            local buf = api.nvim_win_get_buf(win)
            local ft = vim.bo[buf].filetype
            if ft == "minibuffer_input" or ft == "minibuffer_results" then
                api.nvim_win_close(win, true)
            end
        end
    end
end

local function truncate(text, max)
    if api.nvim_strwidth(text) <= max then
        return text
    end
    if max <= 3 then
        return text:sub(1, math.max(max, 0))
    end
    return vim.fn.strcharpart(text, 0, max - 3) .. "..."
end

local function complete_line(input, selection)
    if vim.startswith(selection, input) then
        return selection
    end

    local prefix = input:match("^(.*[%s%.%/:\\])" or "") or ""
    return prefix .. selection
end

local function subsequence_positions(str, query)
    local positions = {}
    if query == "" then
        return positions
    end

    local s = str:lower()
    local q = query:lower()
    local j = 1

    for i = 1, #s do
        if s:sub(i, i) == q:sub(j, j) then
            table.insert(positions, i)
            j = j + 1
            if j > #q then
                break
            end
        end
    end

    if j <= #q then
        return nil
    end

    return positions
end

local function fuzzy_score(str, query)
    local positions = subsequence_positions(str, query)
    if not positions then
        return nil
    end

    if #query == 0 then
        return 0
    end

    local score = 0
    local last = 0
    for _, pos in ipairs(positions) do
        local distance = pos - last
        score = score + math.max(50 - distance, 1)

        if last > 0 and pos == last + 1 then
            score = score + 12
        end

        if pos == 1 or str:sub(pos - 1, pos - 1):match("[^%w]") then
            score = score + 8
        end

        last = pos
    end

    score = score - math.floor(#str / 10)
    return score
end

local function fuzzy_filter(items_or_provider, query, opts)
    opts = opts or {}

    if type(items_or_provider) == "function" then
        return items_or_provider(query) or {}
    end

    local items = items_or_provider or {}

    if query == "" then
        return items
    end

    if opts.sorter then
        return opts.sorter(items, query) or {}
    end

    if vim.fn.exists("*matchfuzzy") == 1 then
        local ok, matched = pcall(vim.fn.matchfuzzy, items, query)
        if ok and type(matched) == "table" then
            return matched
        end
    end

    local scored = {}
    for _, item in ipairs(items) do
        local s = fuzzy_score(item, query)
        if s then
            scored[#scored + 1] = { item = item, score = s }
        end
    end

    table.sort(scored, function(a, b)
        if a.score == b.score then
            return a.item < b.item
        end
        return a.score > b.score
    end)

    local out = {}
    for _, entry in ipairs(scored) do
        out[#out + 1] = entry.item
    end
    return out
end

local UI = {}
UI.__index = UI

function UI.new(prompt_text)
    local self = setmetatable({}, UI)
    self.base_prompt = prompt_text
    self.ns_id = api.nvim_create_namespace("minibuffer")
    self.prompt_ns = api.nvim_create_namespace("minibuffer_prompt")
    return self
end

function UI:create_windows()
    setup_highlights()

    self.input_buf = api.nvim_create_buf(false, true)
    vim.bo[self.input_buf].filetype = "minibuffer_input"
    vim.bo[self.input_buf].bufhidden = "wipe"

    local width = vim.o.columns
    local height = 1
    local row = vim.o.lines - 2

    self.input_win = api.nvim_open_win(self.input_buf, true, {
        relative = "editor",
        width = width,
        height = height,
        row = row,
        col = 0,
        style = "minimal",
        focusable = true,
        zindex = 150,
    })

    vim.wo[self.input_win].number = false
    vim.wo[self.input_win].relativenumber = false
    vim.wo[self.input_win].signcolumn = "no"
    vim.wo[self.input_win].cursorline = false
    vim.wo[self.input_win].foldcolumn = "0"
    vim.wo[self.input_win].spell = false
    vim.wo[self.input_win].list = false
    vim.wo[self.input_win].winhl = "Normal:MinibufferNormal"

    api.nvim_buf_set_lines(self.input_buf, 0, -1, false, { "" })
    self:update_prompt_virtual_text(self.base_prompt)

    return self.input_buf, self.input_win
end

function UI:update_prompt_virtual_text(text)
    if not (self.input_buf and api.nvim_buf_is_valid(self.input_buf)) then
        return
    end

    api.nvim_buf_clear_namespace(self.input_buf, self.prompt_ns, 0, -1)
    api.nvim_buf_set_extmark(self.input_buf, self.prompt_ns, 0, 0, {
        virt_text = { { text, "MinibufferPrompt" } },
        virt_text_pos = "inline",
        right_gravity = false,
    })
end

local function build_chunks(text, selected, match_positions)
    local chunks = {}
    local match_map = {}

    for _, p in ipairs(match_positions or {}) do
        match_map[p] = true
    end

    for i = 1, #text do
        local ch = text:sub(i, i)
        local hl = match_map[i] and "MinibufferMatchChar"
            or (selected and "MinibufferSelected" or "MinibufferMatch")

        local prev = chunks[#chunks]
        if prev and prev[2] == hl then
            prev[1] = prev[1] .. ch
        else
            chunks[#chunks + 1] = { ch, hl }
        end
    end

    return chunks
end

function UI:render(matches, selected_index, marked, query)
    if not (self.input_buf and api.nvim_buf_is_valid(self.input_buf)) then
        return
    end

    api.nvim_buf_clear_namespace(self.input_buf, self.ns_id, 0, -1)
    self:update_prompt_virtual_text(self.base_prompt)

    local line = api.nvim_buf_get_lines(self.input_buf, 0, 1, false)[1] or ""
    local line_max = math.max(vim.o.columns - 1, 1)
    local current_pos = api.nvim_strwidth(self.base_prompt) + api.nvim_strwidth(line)

    local chunks = {}
    local function add_chunk(text, hl)
        if text == "" then
            return
        end

        local prev = chunks[#chunks]
        if prev and prev[2] == hl then
            prev[1] = prev[1] .. text
        else
            chunks[#chunks + 1] = { text, hl }
        end
    end

    if current_pos + 1 < line_max then
        add_chunk(" ", "MinibufferSeparator")
        current_pos = current_pos + 1
    end

    if #matches == 0 then
        add_chunk("[No matches]", "MinibufferNoMatch")
    else
        local list_suffix = "}"
        if current_pos + 1 < line_max then
            add_chunk("{", "MinibufferSeparator")
            current_pos = current_pos + 1
        else
            list_suffix = ""
        end

        for i, item in ipairs(matches) do
            local sep = i > 1 and " | " or ""
            if current_pos + #sep >= line_max then
                break
            end

            if sep ~= "" then
                add_chunk(sep, "MinibufferSeparator")
                current_pos = current_pos + #sep
            end

            local remaining = line_max - current_pos
            if remaining <= 0 then
                break
            end

            local mark_prefix = (marked and marked[item]) and "* " or ""
            local shown = truncate(mark_prefix .. item, remaining)

            local item_text = shown
            if mark_prefix ~= "" and shown:sub(1, #mark_prefix) == mark_prefix then
                add_chunk(mark_prefix, "MinibufferMark")
                item_text = shown:sub(#mark_prefix + 1)
            end

            local positions = subsequence_positions(item_text, query or "") or {}
            local item_chunks = build_chunks(item_text, i == selected_index, positions)
            for _, c in ipairs(item_chunks) do
                add_chunk(c[1], c[2])
            end

            current_pos = current_pos + api.nvim_strwidth(shown)
            if current_pos >= line_max then
                break
            end
        end

        if list_suffix ~= "" and current_pos + 1 < line_max then
            add_chunk(list_suffix, "MinibufferSeparator")
        end
    end

    api.nvim_buf_set_extmark(self.input_buf, self.ns_id, 0, 0, {
        virt_text = chunks,
        virt_text_pos = "eol",
        right_gravity = false,
    })
end

function UI:update_input(lines)
    api.nvim_buf_set_lines(self.input_buf, 0, -1, false, lines)
    if self.input_win and api.nvim_win_is_valid(self.input_win) then
        api.nvim_win_set_cursor(self.input_win, { 1, #(lines[1] or "") })
    end
end

function UI:close()
    if self.input_win and api.nvim_win_is_valid(self.input_win) then
        api.nvim_win_close(self.input_win, true)
    end
    vim.cmd("stopinsert")
end

function M.get_relative_path(filename)
    local cwd = vim.fn.getcwd()
    if not cwd:match("/$") then
        cwd = cwd .. "/"
    end

    if filename:sub(1, #cwd) == cwd then
        return filename:sub(#cwd + 1)
    end

    return filename
end

function M.parse_selection(selection, format)
    if not selection or selection == "" then
        return nil
    end

    local schema = schemas[format]
    if not schema then
        return nil
    end

    local patterns = type(schema.pattern) == "table" and schema.pattern or { schema.pattern }
    local matches = {}

    for _, pat in ipairs(patterns) do
        matches = { selection:match(pat) }
        if #matches > 0 then
            break
        end
    end

    if #matches == 0 then
        return nil
    end

    local result = {}
    for i, key in ipairs(schema.keys) do
        local val = matches[i]
        if val then
            if schema.types and schema.types[key] then
                val = schema.types[key](val)
            end
            result[key] = val
        end
    end

    result.lnum = result.lnum or 1
    result.col = result.col or 1

    return result
end

M.parsers = {
    file = function(selection)
        return M.parse_selection(selection, "file")
    end,
    grep = function(selection)
        return M.parse_selection(selection, "grep")
    end,
    lsp = function(selection)
        return M.parse_selection(selection, "lsp")
    end,
    buffer = function(selection)
        return M.parse_selection(selection, "buffer")
    end,
}

function M.jump_to_location(selection, data_or_format)
    local data = data_or_format
    if type(data_or_format) == "string" then
        data = M.parse_selection(selection, data_or_format)
    end

    if not (data and data.filename) then
        return
    end

    vim.cmd("edit " .. vim.fn.fnameescape(data.filename))
    if data.lnum and data.col then
        vim.api.nvim_win_set_cursor(0, { data.lnum, data.col - 1 })
    end
end

function M.get_line_content(filename, lnum)
    if vim.fn.filereadable(filename) == 0 then
        return ""
    end

    local bufnr = vim.fn.bufnr(filename)
    if bufnr ~= -1 and vim.api.nvim_buf_is_loaded(bufnr) then
        local lines = vim.api.nvim_buf_get_lines(bufnr, lnum - 1, lnum, false)
        return lines[1] or ""
    end

    local lines = vim.fn.readfile(filename, "", lnum)
    return lines[lnum] or ""
end

function M.pick(items_or_provider, on_select, opts)
    local original_win = api.nvim_get_current_win()
    local original_buf = api.nvim_win_get_buf(original_win)
    local original_cursor = api.nvim_win_get_cursor(original_win)

    close_existing()

    opts = opts or {}
    on_select = on_select or opts.on_select

    local prompt_text = opts.prompt or "> "
    local parser = opts.parser

    if not parser and opts.selection_format then
        parser = function(s)
            return M.parse_selection(s, opts.selection_format)
        end
    end

    local ui = UI.new(prompt_text)
    active_ui = ui
    local input_buf, input_win = ui:create_windows()

    local current_matches = {}
    local marked = {}
    local selected_index = 1
    local is_previewing = false
    local function current_input_line()
        if input_buf and api.nvim_buf_is_valid(input_buf) then
            return api.nvim_buf_get_lines(input_buf, 0, 1, false)[1] or ""
        end
        return ""
    end

    local function preview()
        if #current_matches == 0 then
            return
        end

        local selection = current_matches[selected_index]
        if not selection then
            return
        end

        local data = parser and parser(selection)
        if not (data and data.filename and api.nvim_win_is_valid(original_win)) then
            return
        end

        local filename = data.filename
        local lnum = data.lnum or 1
        local col = data.col or 1

        is_previewing = true
        api.nvim_win_call(original_win, function()
            local bufnr = vim.fn.bufnr(filename)
            if bufnr == -1 then
                bufnr = vim.fn.bufadd(filename)
                vim.fn.bufload(bufnr)
            end

            if bufnr ~= -1 and api.nvim_buf_is_valid(bufnr) then
                api.nvim_win_set_buf(original_win, bufnr)
                pcall(api.nvim_win_set_cursor, original_win, { lnum, col - 1 })
                pcall(vim.cmd, "normal! zz")
            end
        end)
        is_previewing = false
    end

    local function normalize_selected_index()
        if selected_index > #current_matches then
            selected_index = #current_matches
        end
        if selected_index < 1 and #current_matches > 0 then
            selected_index = 1
        end
    end

    local function refresh()
        local input = current_input_line()

        if opts.on_change then
            selected_index = 1
            opts.on_change(input, function(matches)
                current_matches = matches or {}
                normalize_selected_index()
                ui:render(current_matches, selected_index, marked, input)
                preview()
            end)
            return
        end

        current_matches = fuzzy_filter(items_or_provider, input, { sorter = opts.sorter })
        selected_index = 1
        ui:render(current_matches, selected_index, marked, input)
        preview()
    end

    local actions = {}

    local function restore_original_view()
        if not api.nvim_win_is_valid(original_win) then
            return
        end

        pcall(api.nvim_set_current_win, original_win)

        if api.nvim_buf_is_valid(original_buf) and api.nvim_win_get_buf(original_win) ~= original_buf then
            pcall(api.nvim_win_set_buf, original_win, original_buf)
        end

        pcall(api.nvim_win_set_cursor, original_win, original_cursor)
    end

    function actions.refresh()
        refresh()
    end

    function actions.next_item()
        if #current_matches == 0 then
            return
        end
        selected_index = (selected_index % #current_matches) + 1
        ui:render(current_matches, selected_index, marked, current_input_line())
        preview()
    end

    function actions.prev_item()
        if #current_matches == 0 then
            return
        end
        selected_index = ((selected_index - 2) % #current_matches) + 1
        ui:render(current_matches, selected_index, marked, current_input_line())
        preview()
    end

    function actions.complete_selection()
        local selection = current_matches[selected_index]
        if not selection then
            return
        end

        local input = current_input_line()
        ui:update_input({ complete_line(input, selection) })
        refresh()
    end

    function actions.toggle_mark()
        local selection = current_matches[selected_index]
        if not selection then
            return
        end

        marked[selection] = not marked[selection]
        ui:render(current_matches, selected_index, marked, current_input_line())
        actions.next_item()
    end

    function actions.select_input()
        local input = current_input_line()
        ui:close()
        if active_ui == ui then
            active_ui = nil
        end

        restore_original_view()

        if on_select and input ~= "" then
            api.nvim_win_call(original_win, function()
                on_select(input, nil)
            end)
        end
    end

    function actions.select_entry()
        local selection = current_matches[selected_index]
        if not selection then
            return
        end

        ui:close()
        if active_ui == ui then
            active_ui = nil
        end

        local data = parser and parser(selection)
        if on_select then
            restore_original_view()
            api.nvim_win_call(original_win, function()
                on_select(selection, data)
            end)
        end
    end

    function actions.send_to_grep()
        local lines = {}
        for item, is_marked in pairs(marked) do
            if is_marked then
                lines[#lines + 1] = item
            end
        end

        if #lines == 0 then
            local selection = current_matches[selected_index]
            if selection then
                lines[#lines + 1] = selection
            end
        end

        if #lines == 0 then
            return
        end

        ui:close()
        if active_ui == ui then
            active_ui = nil
        end

        local ok, grep_buf = pcall(require, "buffers.grep")
        if ok and grep_buf and grep_buf.create_buffer then
            grep_buf.create_buffer(lines)
        else
            vim.notify("buffers.grep module not found", vim.log.levels.WARN)
        end
    end

    function actions.close()
        restore_original_view()

        ui:close()
        if active_ui == ui then
            active_ui = nil
        end
    end

    local default_keymaps = {
        ["<Tab>"] = "complete_selection",
        ["<C-l>"] = "next_item",
        ["<C-f>"] = "next_item",
        ["<C-h>"] = "prev_item",
        ["<C-b>"] = "prev_item",
        ["<Down>"] = "next_item",
        ["<Up>"] = "prev_item",
        ["<CR>"] = "select_input",
        ["<Esc>"] = "close",
        ["<C-c>"] = "close",
        ["<C-g>"] = "send_to_grep",
    }

    local keymaps = vim.tbl_extend("force", default_keymaps, opts.keymaps or {})

    local parameters = {
        original_win = original_win,
        original_buf = original_buf,
        original_cursor = original_cursor,
    }

    local function map(key, func)
        vim.keymap.set("i", key, func, { buffer = input_buf, silent = true })
    end

    for key, handler in pairs(keymaps) do
        if type(handler) == "string" and actions[handler] then
            map(key, actions[handler])
        elseif type(handler) == "function" then
            map(key, function()
                local selection = current_matches[selected_index]
                if not selection then
                    return
                end
                handler(selection, {
                    actions = actions,
                    parameters = parameters,
                    marked = marked,
                })
            end)
        end
    end

    local group = api.nvim_create_augroup("MinibufferLive", { clear = true })

    api.nvim_create_autocmd("TextChangedI", {
        buffer = input_buf,
        group = group,
        callback = refresh,
    })

    api.nvim_create_autocmd("WinLeave", {
        buffer = input_buf,
        group = group,
        callback = function()
            if is_previewing then
                return
            end

            if api.nvim_win_is_valid(original_win) and api.nvim_buf_is_valid(original_buf) then
                api.nvim_win_set_buf(original_win, original_buf)
                api.nvim_win_set_cursor(original_win, original_cursor)
            end

            ui:close()
            if active_ui == ui then
                active_ui = nil
            end
        end,
    })

    if input_win and api.nvim_win_is_valid(input_win) then
        api.nvim_set_current_win(input_win)
    end

    refresh()
    vim.cmd("startinsert")
end

local current_job = nil

function M.files(opts)
    opts = opts or {}

    local ignored_dirs = opts.ignored_dirs or { ".git", ".jj", "node_modules", ".cache" }
    local cmd = { "fd", "-H", "--type", "f" }
    for _, dir in ipairs(ignored_dirs) do
        table.insert(cmd, "--exclude")
        table.insert(cmd, dir)
    end

    local fd_result = vim.system(cmd, { text = true }):wait()
    local files_list = vim.split(fd_result.stdout or "", "\n", { trimempty = true })

    M.pick(files_list, nil, {
        prompt = opts.prompt or "Files > ",
        keymaps = {
            ["<Tab>"] = "toggle_mark",
            ["<CR>"] = "select_entry",
        },
        parser = M.parsers.file,
        on_select = function(selection, data)
            M.jump_to_location(selection, data)
            pcall(vim.cmd, 'normal! g`"')
        end,
    })
end

function M.live_grep(opts)
    opts = opts or {}

    local function run_async_grep(query, update_ui_callback)
        if current_job then
            current_job:kill()
            current_job = nil
        end

        update_ui_callback({})

        if not query or #query < 2 then
            return
        end

        local cmd = { "rg", "--vimgrep", "--smart-case", "--", query }
        local output_lines = {}
        local this_job

        this_job = vim.system(cmd, {
            text = true,
            stdout = function(_, data)
                if not data then
                    return
                end

                local lines = vim.split(data, "\n", { trimempty = true })
                for _, line in ipairs(lines) do
                    table.insert(output_lines, line)
                end

                vim.schedule(function()
                    if current_job ~= this_job then
                        return
                    end
                    update_ui_callback(output_lines)
                end)
            end,
        })

        current_job = this_job
    end

    M.pick({}, M.jump_to_location, {
        prompt = opts.prompt or "Grep > ",
        parser = M.parsers.grep,
        on_change = run_async_grep,
        keymaps = {
            ["<Tab>"] = "toggle_mark",
            ["<CR>"] = "select_entry",
        },
    })
end

function M.grep(opts)
    M.live_grep(opts)
end

function M.commands(opts)
    opts = opts or {}
    M.pick(function(input)
        if input == "" then
            return vim.fn.getcompletion("", "command")
        end
        return vim.fn.getcompletion(input, "cmdline")
    end, function(input_text)
        vim.cmd(input_text)
    end, {
        prompt = opts.prompt or "M-x > ",
    })
end

function M.buffers(opts)
    opts = opts or {}

    local bufs = vim.api.nvim_list_bufs()
    local items = {}

    for _, bufnr in ipairs(bufs) do
        if vim.bo[bufnr].buflisted then
            local name = vim.api.nvim_buf_get_name(bufnr)
            if name ~= "" then
                local relative_path = M.get_relative_path(name)
                local row_col = vim.api.nvim_buf_get_mark(bufnr, '"')
                if row_col[1] == 0 then
                    row_col[1] = 1
                end
                if row_col[2] == 0 then
                    row_col[2] = 1
                end
                items[#items + 1] = string.format("%d: %s:%d:%d", bufnr, relative_path, row_col[1], row_col[2])
            end
        end
    end

    M.pick(items, M.jump_to_location, {
        prompt = opts.prompt or "Buffers > ",
        keymaps = {
            ["<Tab>"] = "toggle_mark",
            ["<CR>"] = "select_entry",
            ["<C-x>"] = function(selection, builtin)
                local data = M.parsers.buffer(selection)
                if not (data and data.bufnr) then
                    return
                end

                local win = builtin.parameters.original_win
                if win and vim.api.nvim_win_is_valid(win) then
                    local current_view_buf = vim.api.nvim_win_get_buf(win)
                    if current_view_buf == data.bufnr then
                        local scratch = vim.api.nvim_create_buf(false, true)
                        vim.bo[scratch].bufhidden = "wipe"
                        vim.api.nvim_win_set_buf(win, scratch)
                    end
                end

                pcall(vim.api.nvim_buf_delete, data.bufnr, { force = true })

                for i, item in ipairs(items) do
                    if item == selection then
                        table.remove(items, i)
                        break
                    end
                end

                builtin.actions.refresh()
            end,
        },
        parser = M.parsers.buffer,
    })
end

function M.diagnostics(opts)
    opts = opts or {}

    local diags = vim.diagnostic.get(nil)
    local items = {}

    for _, d in ipairs(diags) do
        local bufnr = d.bufnr
        local filename = bufnr and vim.api.nvim_buf_get_name(bufnr) or ""
        if filename ~= "" then
            local rel = M.get_relative_path(filename)
            local lnum = (d.lnum or 0) + 1
            local col = (d.col or 0) + 1
            local msg = (d.message or ""):gsub("[%c]+", " ")
            items[#items + 1] = string.format("%s:%d:%d:%s", rel, lnum, col, msg)
        end
    end

    if #items == 0 then
        vim.notify("No diagnostics found", vim.log.levels.INFO)
        return
    end

    M.pick(items, M.jump_to_location, {
        prompt = opts.prompt or "Diagnostics > ",
        keymaps = {
            ["<Tab>"] = "toggle_mark",
            ["<CR>"] = "select_entry",
        },
        parser = M.parsers.grep,
    })
end

function M.man(opts)
    opts = opts or {}

    local function unique(tbl)
        local seen, out = {}, {}
        for _, v in ipairs(tbl or {}) do
            if v ~= "" and not seen[v] then
                seen[v] = true
                out[#out + 1] = v
            end
        end
        return out
    end

    M.pick(function(input)
        local raw = vim.fn.getcompletion("Man " .. (input or ""), "cmdline")
        local out = {}
        for _, item in ipairs(raw) do
            out[#out + 1] = item:gsub("^Man%s+", "")
        end
        return unique(out)
    end, function(topic)
        if topic and topic ~= "" then
            vim.cmd("Man " .. topic)
        end
    end, {
        prompt = opts.prompt or "Man > ",
        keymaps = {
            ["<CR>"] = "select_input",
        },
    })
end

function M.git_diff(opts)
    opts = opts or {}

    local function syslist(cmd)
        local out = vim.system(cmd, { text = true }):wait()
        if out.code ~= 0 then
            return {}
        end
        return vim.split(out.stdout or "", "\n", { trimempty = true })
    end

    local files = {}
    local seen = {}
    for _, cmd in ipairs({
        { "git", "diff", "--name-only", "--" },
        { "git", "diff", "--cached", "--name-only", "--" },
        { "git", "ls-files", "--others", "--exclude-standard" },
    }) do
        for _, f in ipairs(syslist(cmd)) do
            if not seen[f] then
                seen[f] = true
                files[#files + 1] = f
            end
        end
    end

    if #files == 0 then
        vim.notify("No git changes found", vim.log.levels.INFO)
        return
    end

    M.pick(files, nil, {
        prompt = opts.prompt or "Git diff > ",
        keymaps = {
            ["<Tab>"] = "toggle_mark",
            ["<CR>"] = "select_entry",
        },
        parser = M.parsers.file,
        on_select = function(selection, data)
            M.jump_to_location(selection, data)
            pcall(vim.cmd, 'normal! g`"')
        end,
    })
end

return M
