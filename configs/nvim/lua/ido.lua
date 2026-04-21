local M = {}

local Async = require("snacks.picker.util.async")
local config = require("snacks.picker.config")
local Filter = require("snacks.picker.core.filter")
local util = require("snacks.picker.util")

local ns = vim.api.nvim_create_namespace("ido_highlight")

local function get_project_root()
  return vim.fn.getcwd()
end

local function get_relative_path(file, root)
  root = root or get_project_root()
  if file:sub(1, #root + 1) == root .. "/" then
    return file:sub(#root + 2)
  end
  return file
end

local function build_ctx(picker, filter)
  local notified = false
  local Ctx = {}
  Ctx.__index = Ctx

  function Ctx.new(p, f)
    local self = setmetatable({}, Ctx)
    self.picker = p
    self.filter = f
    self.meta = {}
    self.async = setmetatable({}, {
      __index = function()
        if not notified then
          notified = true
          Snacks.notify.warn("You can only use the `async` object in async functions")
        end
      end,
    })
    return self
  end

  function Ctx:clone(opts)
    return setmetatable({ _opts = opts }, { __index = self })
  end

  function Ctx:opts(opts)
    self._opts = setmetatable(opts or {}, { __index = self._opts or self.picker.opts })
    return self._opts
  end

  function Ctx:cwd()
    return self.filter.cwd
  end

  function Ctx:git_root()
    return Snacks.git.get_root(self:cwd()) or self:cwd()
  end

  return Ctx.new(picker, filter)
end

local function truncate(text, max)
  if vim.api.nvim_strwidth(text) <= max then
    return text
  end
  if max <= 1 then
    return "…"
  end
  return vim.fn.strcharpart(text, 0, max - 1) .. "…"
end

local function default_display(item, root)
  local path = util.path(item)
  if path then
    return get_relative_path(path, root)
  end
  if item.buf and vim.api.nvim_buf_is_valid(item.buf) then
    local name = vim.api.nvim_buf_get_name(item.buf)
    if name ~= "" then
      return get_relative_path(name, root)
    end
  end
  if item.text then
    return item.text
  end
  if item.label then
    return item.label
  end
  return tostring(item)
end

local function match_list(list, pattern, limit)
  if pattern == "" then
    return list
  end
  if vim.fn.exists("*matchfuzzy") == 1 then
    if limit and type(limit) == "number" then
      local ok, res = pcall(vim.fn.matchfuzzy, list, pattern, { limit = limit })
      if ok then
        return res
      end
    end
    return vim.fn.matchfuzzy(list, pattern)
  end
  local out = {}
  local needle = pattern:lower()
  for _, s in ipairs(list) do
    if s:lower():find(needle, 1, true) then
      out[#out + 1] = s
      if limit and #out >= limit then
        break
      end
    end
  end
  return out
end

local function open_preview(main_win, item)
  if not (main_win and vim.api.nvim_win_is_valid(main_win) and item) then
    return
  end
  local buf = nil
  if item.buf and vim.api.nvim_buf_is_valid(item.buf) then
    buf = item.buf
  else
    local path = util.path(item)
    if path then
      buf = vim.fn.bufadd(path)
      vim.fn.bufload(buf)
    end
  end
  if not buf or not vim.api.nvim_buf_is_valid(buf) then
    return
  end
  pcall(vim.api.nvim_win_set_buf, main_win, buf)
  if item.pos and item.pos[1] then
    pcall(vim.api.nvim_win_set_cursor, main_win, { item.pos[1], (item.pos[2] or 1) - 1 })
  end
end

-- Function to get match positions from fzf
local function get_match_positions(str, query)
  local positions = {}
  local query_chars = {}
  for c in query:lower():gmatch(".") do
    table.insert(query_chars, c)
  end

  local j = 1
  for i = 1, #str do
    local char = str:sub(i, i):lower()
    if j <= #query_chars and char == query_chars[j] then
      table.insert(positions, i - 1)
      j = j + 1
    end
  end

  return positions
end

-- Main ido-style fuzzy finder function
function M.open(opts)
  opts = opts or {}
  local source = opts.source or "smart"
  local base_prompt = opts.prompt or "Find file: "
  local max_items = opts.max_items or 10
  local root = opts.root or get_project_root()
  local include_root = opts.include_root ~= false
  local prompt = base_prompt
  local function update_prompt()
    if include_root then
      local root_disp = vim.fn.fnamemodify(root, ":~")
      prompt = base_prompt .. root_disp .. "/"
    else
      prompt = base_prompt
    end
  end
  update_prompt()

  local ok_opts, picker_opts = pcall(config.get, { source = source })
  if not ok_opts or not picker_opts then
    Snacks.notify.error("Ido picker: failed to resolve source config")
    return
  end
  picker_opts.pattern = ""
  picker_opts.search = ""

  local picker = { opts = picker_opts }
  local filter = Filter.new(picker)
  filter.pattern = ""
  filter.search = ""

  local ctx = build_ctx(picker, filter)
  local transform = config.transform(picker_opts)
  local finder = config.finder(picker_opts.finder)

  local main_win = vim.api.nvim_get_current_win()
  local main_buf = vim.api.nvim_win_get_buf(main_win)
  local main_view = vim.api.nvim_win_call(main_win, vim.fn.winsaveview)

  local current_input = ""
  local selected_index = 1

  local state = {
    items = {},
    indexed = {},
    matches = {},
    loaded = false,
    closed = false,
    refresh_scheduled = false,
    finder_seq = 0,
  }

  -- Create buffer and window for ido-style interface
  local buf = vim.api.nvim_create_buf(false, true)
  local width = vim.o.columns
  local height = 1
  local win = vim.api.nvim_open_win(buf, true, {
    relative = 'editor',
    width = width,
    height = height,
    row = vim.o.lines - 2,
    col = 0,
    style = 'minimal',
    focusable = true,
    zindex = 150
  })

  -- Function to update the display
  local function close_picker(restore)
    if state.closed then
      return
    end
    state.closed = true
    if restore and vim.api.nvim_win_is_valid(main_win) then
      pcall(vim.api.nvim_win_set_buf, main_win, main_buf)
      pcall(vim.api.nvim_win_call, main_win, function()
        vim.fn.winrestview(main_view)
      end)
    end
    if vim.api.nvim_win_is_valid(win) then
      pcall(vim.api.nvim_win_close, win, true)
    end
    if vim.api.nvim_buf_is_valid(buf) then
      pcall(vim.api.nvim_buf_delete, buf, { force = true })
    end
  end

  local function update_display()
    local prompt_prefix = prompt .. current_input .. " "
    local display_text = prompt_prefix
    local line_max = math.max(vim.o.columns - 1, 1)

    if #state.matches > 0 then
      local display_matches = {}
      local current_pos = #prompt_prefix
      local list_prefix = "{"
      local list_suffix = "}"
      if current_pos + #list_prefix >= line_max then
        list_prefix = ""
        list_suffix = ""
      else
        table.insert(display_matches, list_prefix)
        current_pos = current_pos + #list_prefix
      end
      for i, item in ipairs(state.matches) do
        local rel_path = item._ido_disp or ""
        local shown = rel_path
        local sep = i > 1 and " | " or ""
        if current_pos + #sep >= line_max then
          break
        end
        if sep ~= "" then
          table.insert(display_matches, sep)
          current_pos = current_pos + #sep
        end
        local remaining = line_max - current_pos
        if remaining <= 0 then
          break
        end
        shown = truncate(shown, remaining)
        table.insert(display_matches, shown)
        current_pos = current_pos + #shown
        if current_pos >= line_max then
          break
        end
      end
      if list_suffix ~= "" and current_pos + #list_suffix < line_max then
        table.insert(display_matches, list_suffix)
      end
      display_text = display_text .. table.concat(display_matches, "")
    else
      display_text = display_text .. (state.loaded and "[No matches]" or "[Searching...]")
    end

    vim.api.nvim_buf_set_lines(buf, 0, -1, false, { display_text })

    vim.api.nvim_buf_clear_namespace(buf, ns, 0, -1)
    vim.api.nvim_buf_add_highlight(buf, ns, "IdoPrompt", 0, 0, #prompt_prefix)

    if #state.matches > 0 then
      local current_pos = #prompt_prefix
      local list_prefix = "{"
      if current_pos + #list_prefix >= line_max then
        list_prefix = ""
      else
        current_pos = current_pos + #list_prefix
      end
      for i, item in ipairs(state.matches) do
        local rel_path = item._ido_disp or ""
        local match_pos = get_match_positions(rel_path, current_input)
        local sep = i > 1 and " | " or ""
        if current_pos + #sep >= line_max then
          break
        end
        current_pos = current_pos + #sep
        if current_pos >= line_max then
          break
        end
        local shown = rel_path
        shown = truncate(shown, line_max - current_pos)

        if i == selected_index then
          vim.api.nvim_buf_add_highlight(buf, ns, "IdoSelected", 0, current_pos, current_pos + #shown)
        else
          vim.api.nvim_buf_add_highlight(buf, ns, "IdoMatch", 0, current_pos, current_pos + #shown)
        end

        local offset = 0
        for _, pos in ipairs(match_pos) do
          local col = current_pos + pos + offset
          if col < current_pos + #shown then
            vim.api.nvim_buf_add_highlight(buf, ns, "IdoMatchChar", 0, col, col + 1)
          end
        end

        current_pos = current_pos + #shown
        if current_pos >= line_max then
          break
        end
      end
    else
      vim.api.nvim_buf_add_highlight(buf, ns, "IdoNoMatch", 0, #prompt_prefix, #display_text)
    end
  end

  -- Function to filter matches using Snacks backend results
  local function update_matches()
    if state.closed then
      return
    end
    if vim.in_fast_event() then
      vim.schedule(update_matches)
      return
    end
    local matched = match_list(state.indexed, current_input, max_items)
    local matches = {}
    for _, raw in ipairs(matched or {}) do
      local idx = tonumber(raw:match("^(%d+)\t"))
      if idx and state.items[idx] then
        matches[#matches + 1] = state.items[idx]
      end
    end
    state.matches = matches
    if selected_index > #matches then
      selected_index = #matches
    end
    if selected_index < 1 then
      selected_index = 1
    end
    update_display()
    open_preview(main_win, state.matches[selected_index])
  end

  local function schedule_refresh()
    if state.refresh_scheduled then
      return
    end
    state.refresh_scheduled = true
    vim.defer_fn(function()
      state.refresh_scheduled = false
      update_matches()
    end, 50)
  end

  local function schedule_display()
    if state.refresh_scheduled then
      return
    end
    state.refresh_scheduled = true
    vim.defer_fn(function()
      state.refresh_scheduled = false
      if state.closed then
        return
      end
      update_display()
      open_preview(main_win, state.matches[selected_index])
    end, 50)
  end

  local function add_item(item)
    if not filter:match(item) then
      return
    end
    if transform then
      local t = transform(item, ctx)
      if t == false then
        return
      end
      if type(t) == "table" then
        item = t
      end
    end
    state.items[#state.items + 1] = item
    item._ido_disp = default_display(item, root)
    state.indexed[#state.indexed + 1] = tostring(#state.items) .. "\t" .. item._ido_disp
    if current_input == "" then
      if #state.matches < max_items then
        state.matches[#state.matches + 1] = item
        schedule_display()
      end
    else
      schedule_refresh()
    end
  end

  local function start_finder()
    picker_opts.cwd = root
    picker_opts.filter = { cwd = true }
    filter = Filter.new(picker)
    filter.pattern = ""
    filter.search = ""
    ctx = build_ctx(picker, filter)
    state.finder_seq = state.finder_seq + 1
    local seq = state.finder_seq
    local function add_current(item)
      if seq ~= state.finder_seq then
        return
      end
      add_item(item)
    end
    local ok, res = pcall(finder, picker_opts, ctx)
    if not ok then
      Snacks.notify.error("Ido picker: finder failed")
      state.loaded = true
      update_matches()
      return
    end
    if type(res) == "table" then
      for _, item in ipairs(res) do
        add_current(item)
      end
      state.loaded = true
      update_matches()
      return
    end
    if type(res) ~= "function" then
      state.loaded = true
      update_matches()
      return
    end
    Async.new(function()
      res(function(item)
        add_current(item)
      end)
    end):on("done", function()
      if seq ~= state.finder_seq then
        return
      end
      state.loaded = true
      update_matches()
    end)
  end

  -- Set up keymaps
  local function setup_keymaps()
    local opts = { noremap = true, silent = true }

    -- Handle character input
    for i = 32, 126 do
      local char = string.char(i)
      vim.api.nvim_buf_set_keymap(buf, 'n', char, '', {
        callback = function()
        current_input = current_input .. char
        schedule_refresh()
      end,
      noremap = true
    })
    end

    -- Handle backspace
    vim.api.nvim_buf_set_keymap(buf, 'n', '<BS>', '', {
      callback = function()
        if current_input == "" then
          local parent = vim.fn.fnamemodify(root, ":h")
          if parent ~= "" and parent ~= root then
            root = parent
            update_prompt()
            current_input = ""
            selected_index = 1
            state.items = {}
            state.indexed = {}
            state.matches = {}
            state.loaded = false
            start_finder()
            update_display()
          end
        else
          current_input = string.sub(current_input, 1, -2)
          schedule_refresh()
        end
      end,
      noremap = true
    })

    -- Handle navigation
    vim.api.nvim_buf_set_keymap(buf, 'n', '<C-n>', '', {
      callback = function()
        if #state.matches > 0 then
          selected_index = selected_index % #state.matches + 1
          update_display()
          open_preview(main_win, state.matches[selected_index])
        end
      end,
      noremap = true
    })

    vim.api.nvim_buf_set_keymap(buf, 'n', '<C-l>', '', {
      callback = function()
        if #state.matches > 0 then
          selected_index = selected_index % #state.matches + 1
          update_display()
          open_preview(main_win, state.matches[selected_index])
        end
      end,
      noremap = true
    })

    vim.api.nvim_buf_set_keymap(buf, 'n', '<C-p>', '', {
      callback = function()
        if #state.matches > 0 then
          selected_index = (selected_index - 2) % #state.matches + 1
          update_display()
          open_preview(main_win, state.matches[selected_index])
        end
      end,
      noremap = true
    })

    vim.api.nvim_buf_set_keymap(buf, 'n', '<C-h>', '', {
      callback = function()
        if #state.matches > 0 then
          selected_index = (selected_index - 2) % #state.matches + 1
          update_display()
          open_preview(main_win, state.matches[selected_index])
        end
      end,
      noremap = true
    })

    vim.api.nvim_buf_set_keymap(buf, 'n', '<Tab>', '', {
      callback = function()
        if #state.matches == 0 then
          return
        end
        local item = state.matches[selected_index]
        local path = util.path(item)
        if not path then
          return
        end
        local abs = vim.fn.fnamemodify(path, ":p"):gsub("/+$", "")
        local root_abs = vim.fn.fnamemodify(root, ":p"):gsub("/+$", "")
        if abs:sub(1, #root_abs) == root_abs then
          local rel = abs:sub(#root_abs + 1)
          if rel:sub(1, 1) == "/" then
            rel = rel:sub(2)
          end
          local first = rel:match("^([^/]+)/")
          if first then
            root = root_abs .. "/" .. first
            update_prompt()
            current_input = ""
            selected_index = 1
            state.items = {}
            state.indexed = {}
            state.matches = {}
            state.loaded = false
            start_finder()
            update_display()
            return
          end
        end
        vim.api.nvim_win_call(main_win, function()
          vim.cmd('edit ' .. vim.fn.fnameescape(path))
        end)
        close_picker(false)
      end,
      noremap = true
    })

    vim.api.nvim_buf_set_keymap(buf, 'n', '<Down>', '', {
      callback = function()
        if #state.matches > 0 then
          selected_index = selected_index % #state.matches + 1
          update_display()
          open_preview(main_win, state.matches[selected_index])
        end
      end,
      noremap = true
    })

    vim.api.nvim_buf_set_keymap(buf, 'n', '<Up>', '', {
      callback = function()
        if #state.matches > 0 then
          selected_index = (selected_index - 2) % #state.matches + 1
          update_display()
          open_preview(main_win, state.matches[selected_index])
        end
      end,
      noremap = true
    })

    -- Handle selection
    vim.api.nvim_buf_set_keymap(buf, 'n', '<CR>', '', {
      callback = function()
        if #state.matches > 0 then
          local item = state.matches[selected_index]
          local path = util.path(item)
          if path then
            vim.api.nvim_win_call(main_win, function()
              vim.cmd('edit ' .. vim.fn.fnameescape(path))
            end)
          elseif item.buf and vim.api.nvim_buf_is_valid(item.buf) then
            vim.api.nvim_win_set_buf(main_win, item.buf)
          end
          if item.pos and item.pos[1] then
            pcall(vim.api.nvim_win_set_cursor, main_win, { item.pos[1], (item.pos[2] or 1) - 1 })
          end
          close_picker(false)
        end
      end,
      noremap = true
    })

    -- Handle cancellation
    vim.api.nvim_buf_set_keymap(buf, 'n', '<Esc>', '', {
      callback = function()
        close_picker(true)
      end,
      noremap = true
    })
  end

  -- Set up highlight groups for the ido window
  vim.cmd([[
        highlight IdoNormal guibg=#1a1b26 guifg=#a9b1d6
        highlight IdoPrompt guifg=#7aa2f7 gui=bold
        highlight IdoSelected guibg=#283457 guifg=#9ece6a gui=bold
        highlight IdoMatch guifg=#7dcfff
        highlight IdoNoMatch guifg=#f7768e gui=italic
        highlight IdoMatchChar guifg=#ff9e64 gui=bold
    ]])

  -- Initialize
  vim.api.nvim_win_set_option(win, 'winhl', 'Normal:IdoNormal')
  setup_keymaps()
  update_display()
  start_finder()
end

function M.ido_find_file()
  M.open({ source = "files" })
end

vim.cmd([[command! IdoFindFile lua require'ido'.ido_find_file()]])

return M
