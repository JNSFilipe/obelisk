local api = vim.api

local M = {}

local ns = api.nvim_create_namespace("avy_overlay")

local cfg = {
    timeout_ms = 1200,
    highlight = "IncSearch",
    label_highlight = "Search",
}

local state = {
    active = false,
    token = 0,
    kind = nil,
    char = nil,
    needle = nil,
    direction = 1,
    targets = nil,
    buf = nil,
    win = nil,
    maps_installed = false,
    group = nil,
    suspend = false,
}

local function feed_key(key)
    if not key or key == "" then
        return
    end
    local tc = api.nvim_replace_termcodes(key, true, false, true)
    api.nvim_feedkeys(tc, "n", false)
end

local function clear_overlay(buf)
    if buf and api.nvim_buf_is_valid(buf) then
        api.nvim_buf_clear_namespace(buf, ns, 0, -1)
    end
end

local function is_word_char(ch)
    return ch ~= nil and ch ~= "" and ch:match("[%w_]") ~= nil
end

local function is_word_match(line, idx, len)
    local before = idx > 1 and line:sub(idx - 1, idx - 1) or ""
    local after = line:sub(idx + len, idx + len)
    return not is_word_char(before) and not is_word_char(after)
end

local function collect_targets(needle, direction, kind)
    local buf = api.nvim_get_current_buf()
    local pos = api.nvim_win_get_cursor(0)
    local start_row = pos[1] - 1
    local start_col = pos[2]
    local line_count = api.nvim_buf_line_count(buf)
    local targets = {}
    local len = #needle

    if direction == -1 then
        for row = start_row, 0, -1 do
            local line = api.nvim_buf_get_lines(buf, row, row + 1, false)[1] or ""
            local max_col = #line
            if row == start_row then
                if kind == "word" then
                    max_col = start_col
                else
                    max_col = start_col
                end
            end
            local matches = {}
            local search_from = 1

            while true do
                local idx = line:find(needle, search_from, true)
                if not idx or idx > max_col then
                    break
                end
                if idx + len - 1 <= max_col and (kind ~= "word" or is_word_match(line, idx, len)) then
                    matches[#matches + 1] = idx
                end
                search_from = idx + 1
            end

            for i = #matches, 1, -1 do
                targets[#targets + 1] = {
                    line = row + 1,
                    col = matches[i] - 1,
                }
                if #targets >= 9 then
                    break
                end
            end

            if #targets >= 9 then
                break
            end
        end
    else
        for row = start_row, line_count - 1 do
            local line = api.nvim_buf_get_lines(buf, row, row + 1, false)[1] or ""
            local search_from = 1
            if row == start_row then
                if kind == "word" then
                    search_from = start_col + 2
                else
                    search_from = start_col + 2
                end
            end

            while #targets < 9 do
                local idx = line:find(needle, search_from, true)
                if not idx then
                    break
                end

                if kind ~= "word" or is_word_match(line, idx, len) then
                    targets[#targets + 1] = {
                        line = row + 1,
                        col = idx - 1,
                    }
                end

                search_from = idx + 1
            end

            if #targets >= 9 then
                break
            end
        end
    end

    return targets
end

local function render_overlay(buf, targets)
    clear_overlay(buf)
    for i, target in ipairs(targets) do
        api.nvim_buf_set_extmark(buf, ns, target.line - 1, target.col, {
            virt_text = { { tostring(i), cfg.highlight } },
            virt_text_pos = "overlay",
            hl_mode = "replace",
            end_col = target.col + 1,
            hl_group = cfg.label_highlight,
            priority = 220,
        })
    end
    vim.cmd("redraw")
end

local function remove_digit_maps(buf)
    if not buf or not api.nvim_buf_is_valid(buf) then
        return
    end

    for _, mode in ipairs({ "n", "x" }) do
        for i = 1, 9 do
            pcall(vim.keymap.del, mode, tostring(i), { buffer = buf })
        end
        pcall(vim.keymap.del, mode, "<Esc>", { buffer = buf })
        pcall(vim.keymap.del, mode, ";", { buffer = buf })
    end
end

local function arm_timeout()
    state.token = state.token + 1
    local current = state.token

    vim.defer_fn(function()
        if state.active and state.token == current then
            M.stop()
        end
    end, cfg.timeout_ms)
end

function M.stop()
    if not state.active and not state.maps_installed then
        return
    end

    clear_overlay(state.buf)
    remove_digit_maps(state.buf)

    if state.group then
        pcall(api.nvim_del_augroup_by_id, state.group)
    end

    state.active = false
    state.token = state.token + 1
    state.kind = nil
    state.char = nil
    state.needle = nil
    state.direction = 1
    state.targets = nil
    state.buf = nil
    state.win = nil
    state.maps_installed = false
    state.group = nil
    state.suspend = false
end

function M.jump(index)
    if not state.active or not state.targets then
        return
    end

    local target = state.targets[index]
    if not target then
        return
    end

    local needle = state.needle
    local kind = state.kind
    state.suspend = true
    clear_overlay(state.buf)

    if state.group then
        pcall(api.nvim_del_augroup_by_id, state.group)
        state.group = nil
    end

    if state.win and api.nvim_win_is_valid(state.win) then
        pcall(api.nvim_set_current_win, state.win)
    end

    if not pcall(api.nvim_win_set_cursor, 0, { target.line, target.col }) then
        state.suspend = false
        M.stop()
        return
    end

    -- Rearm after the jump so pressing the same digit advances repeatedly.
    vim.schedule(function()
        if not state.active then
            return
        end
        M.start_cycle(needle, kind)
        state.suspend = false
    end)
end

function M.start_cycle(needle, kind)
    if not needle or needle == "" then
        M.stop()
        return
    end

    local win = api.nvim_get_current_win()
    local buf = api.nvim_get_current_buf()
    if not api.nvim_win_is_valid(win) or not api.nvim_buf_is_valid(buf) then
        M.stop()
        return
    end

    kind = kind or state.kind or "char"
    local direction = state.direction == -1 and -1 or 1
    local targets = collect_targets(needle, direction, kind)
    if #targets == 0 then
        M.stop()
        return
    end

    state.active = true
    state.kind = kind
    state.char = kind == "char" and needle or nil
    state.needle = needle
    state.direction = direction
    state.targets = targets
    state.buf = buf
    state.win = win

    if not state.maps_installed then
        for _, mode in ipairs({ "n", "x" }) do
            for i = 1, 9 do
                vim.keymap.set(mode, tostring(i), function()
                    require("avy").jump(i)
                end, {
                    buffer = buf,
                    silent = true,
                    nowait = true,
                    desc = "Avy jump " .. i,
                })
            end

            vim.keymap.set(mode, "<Esc>", function()
                require("avy").stop()
                feed_key("<Esc>")
            end, {
                buffer = buf,
                silent = true,
                nowait = true,
                desc = "Avy cancel",
            })

            vim.keymap.set(mode, ";", function()
                require("avy").invert_direction()
            end, {
                buffer = buf,
                silent = true,
                nowait = true,
                desc = "Avy invert direction",
            })
        end
        state.maps_installed = true
    end

    state.group = api.nvim_create_augroup("AvyActive", { clear = true })
    api.nvim_create_autocmd({ "CursorMoved", "ModeChanged", "InsertEnter", "BufLeave", "WinLeave" }, {
        group = state.group,
        buffer = buf,
        callback = function()
            if not state.suspend then
                require("avy").stop()
            end
        end,
    })

    render_overlay(buf, targets)
    arm_timeout()
end

function M.invert_direction()
    if not state.active then
        return
    end
    state.direction = (state.direction == 1) and -1 or 1
    M.start_cycle(state.needle, state.kind)
end

function M.trigger()
    local char = vim.fn.getcharstr()
    if not char or char == "" or char == "\027" then
        return
    end

    M.stop()
    state.direction = 1
    state.suspend = true
    M.start_cycle(char, "char")
    vim.schedule(function()
        state.suspend = false
    end)
end

function M.trigger_word_select()
    local word = vim.fn.expand("<cword>")

    if not word or word == "" then
        return
    end

    M.stop()
    state.direction = 1
    state.suspend = true
    vim.cmd("normal! viw")
    M.start_cycle(word, "word")
    vim.schedule(function()
        state.suspend = false
    end)
end

-- Backward-compatible alias
M.trigger_word_delete = M.trigger_word_select

function M.setup(opts)
    cfg = vim.tbl_extend("force", cfg, opts or {})

    local function set_hl()
        vim.api.nvim_set_hl(0, "AvyOverlay", { link = "IncSearch" })
        vim.api.nvim_set_hl(0, "AvyLabel", { link = "Search" })
    end

    set_hl()
    cfg.highlight = "AvyOverlay"
    cfg.label_highlight = "AvyLabel"

    vim.api.nvim_create_autocmd("ColorScheme", {
        group = vim.api.nvim_create_augroup("AvyHL", { clear = true }),
        callback = set_hl,
    })

    vim.keymap.set({ "n", "x" }, "f", function()
        require("avy").trigger()
    end, { desc = "Avy jump (1-9)", silent = true })
end

return M
