-- Inspirations
-- https://github.com/EvWilson/slimux.nvim

local tmux = require('tmux')

-- [[ Configuration ]] --
local fextensions = { -- TODO: Find more recent REPLs
  python = {
    command = "rlwrap python | pygmentize -s -l python -O style=native",
    pane_name = "Python REPL",
  },
  lisp = {
    command = "rlwrap -i -b '()' sbcl | pygmentize -s -l lisp -O style=native",
    pane_name = "Common Lisp REPL",
  },
}

-- [[ Helper functions ]] --

local function escape_trailing_semicolons(text)
  local escaped = string.gsub(text, ';\n', '\\;\n')
  if string.sub(escaped, -1) == ';' then
    escaped = string.sub(escaped, 1, -2) .. '\\;'
  end
  return escaped
end

local function remove_comments(code_str, language)
  if language == "lua" then
    -- Removes Lua comments (single line --)
    return code_str:gsub('%-%-[^\n]*', '')
  elseif language == "lisp" then
    -- Removes Common Lisp comments (single line ;;)
    return code_str:gsub('%;%;[^\n]*', '')
  elseif language == "py" then
    -- Removes Python comments (single line #)
    return code_str:gsub('%#[^\n]*', '')
  else
    return code_str
  end
end

local function fix_indentation(text)
  -- Replace tabs with spaces (assuming a tab width of 4 spaces)
  local tab_width = 2
  local text_with_spaces = text:gsub("\t", string.rep(" ", tab_width))

  -- Determine the indentation of the first line
  local first_line_indent = text_with_spaces:match("^(%s*)")
  if not first_line_indent then return text_with_spaces end

  -- Remove the first line's indentation from all lines
  local unindented_text = text_with_spaces:gsub("\n" .. first_line_indent, "\n"):gsub("^" .. first_line_indent, "")
  return unindented_text
end

local function send(text)
  local lang = vim.bo.filetype
  text = remove_comments(text, lang)
  text = text:gsub('"', "'")
  text = escape_trailing_semicolons(text)
  local cmd = string.format('tmux send-keys -t %s -- "%s" Enter', tmux.get_id_from_name(fextensions[lang].pane_name),
    text)
  if lang == "python" then
    cmd = cmd .. " Enter"
  end
  vim.fn.systemlist(cmd)
end



-- [[ API ]] --

M = {}

function M.start_repl()
  local lang = vim.bo.filetype
  tmux.create_or_move_tmux_pane({
    command = fextensions[lang].command,
    pane_name = fextensions[lang].pane_name,
    split_direction = "h",
    pane_size = 30,
  })
end

function M.capture_selection()
  local mode = vim.fn.visualmode()
  vim.cmd('noau normal! "vy"') -- yank the visual selection to the v register

  local selection = vim.fn.getreg('v')
  vim.fn.setreg('v', {}) -- clear the register

  -- For block visual mode, remove trailing newlines
  if mode == "\22" then -- ^V or CTRL-V is blockwise-visual
    selection = selection:gsub('\n$', '')
  end

  return selection
end

function M.capture_line()
  return vim.api.nvim_get_current_line()
end

function M.capture_function()
  local parsers = require 'nvim-treesitter.parsers'
  local ts_utils = require 'nvim-treesitter.ts_utils'

  if not parsers.has_parser() then return nil end

  local current_node = ts_utils.get_node_at_cursor()
  if not current_node then return nil end

  while current_node do
    local node_type = current_node:type()
    -- Check for function nodes in both Python and Common Lisp
    if node_type == 'function_definition' -- Python
        or node_type == 'defun'           -- Common Lisp
        or node_type == 'lambda'          -- Common Lisp
        or node_type == 'sexp' then       -- Common Lisp
      local start_row, start_col, end_row, end_col = current_node:range()
      local lines = vim.api.nvim_buf_get_lines(0, start_row, end_row + 1, false)
      return table.concat(lines, '\n')
    end
    current_node = current_node:parent()
  end

  return nil
end

function M.capture_outermost_expression()
  local current_buffer = vim.api.nvim_get_current_buf()
  local cursor_line = vim.api.nvim_win_get_cursor(0)[1]
  local start_line, end_line = cursor_line, cursor_line
  while start_line > 0 do
    local line = vim.api.nvim_buf_get_lines(current_buffer, start_line - 1, start_line, false)[1]
    if line == "" then
      break
    end
    start_line = start_line - 1
  end
  local total_lines = vim.api.nvim_buf_line_count(current_buffer)
  while end_line < total_lines do
    local line = vim.api.nvim_buf_get_lines(current_buffer, end_line, end_line + 1, false)[1]
    if line == "" then
      break
    end
    end_line = end_line + 1
  end
  local paragraph_lines = vim.api.nvim_buf_get_lines(current_buffer, start_line, end_line, false)
  local paragraph_text = table.concat(paragraph_lines, '\n')
  return paragraph_text
end

function M.capture_buffer()
  local bufnr = vim.api.nvim_get_current_buf()                          -- Get the current buffer number
  local line_count = vim.api.nvim_buf_line_count(bufnr)                 -- Get the number of lines in the buffer
  local lines = vim.api.nvim_buf_get_lines(bufnr, 0, line_count, false) -- Get all lines in the buffer
  return table.concat(lines, '\n')                                      -- Concatenate lines with newline character
end

function M.send_to_tmux_repl(capturer)
  if capturer == nil then
    capturer = M.capture_selection
  end
  local hl = capturer()
  hl = remove_comments(hl, vim.bo.filetype)
  hl = fix_indentation(hl)
  send(hl)
end

return M
