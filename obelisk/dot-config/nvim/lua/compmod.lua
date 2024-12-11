-- NOTE: This might someday be a standalone pluggin
-- TODO:
-- [ ] Make message saying what the program returns colorful
-- [ ] Make compilation buffer visible in :Telescope buffers
-- [ ] Change higlight color from blue
-- [ ] Change navigation shortcuts from n and N to º and ª
-- [ ] See why outputs of C programs only appear when writing to stderr
-- [ ] Make a prompt to specifically run ripgrep, cause it is fun
-- [ ] Integrate with make/telescope

local M = {}

-- Global variable to track active processes
local active_process = nil

-- Helper function to safely close a buffer
local function safe_close_buffer(buf)
  if vim.api.nvim_buf_is_valid(buf) then
    pcall(vim.api.nvim_buf_delete, buf, { force = true })
  end
end

-- Helper function to terminate a running process
local function terminate_process(process)
  if process and process.handle and not process.handle:is_closing() then
    -- First, try to terminate gracefully
    local pid = process.pid
    if pid then
      -- Send SIGTERM first
      os.execute(string.format("kill -TERM %d", pid))

      -- Wait a short moment
      vim.loop.sleep(100)

      -- If still running, send SIGKILL
      os.execute(string.format("kill -KILL %d", pid))
    end

    -- Close the handle
    pcall(process.handle.close, process.handle)
  end
end

-- Helper function to prompt user about terminating existing process
local function prompt_terminate_process()
  local choice = vim.fn.confirm(
    "A process is already running. Do you want to stop it?",
    "&Yes\n&No",
    1
  )
  return choice == 1
end

-- Helper function to safely close an active process
local function cleanup_active_process()
  if active_process then
    -- Terminate the running process
    terminate_process(active_process)

    -- Close buffer if it exists
    if active_process.buffer and vim.api.nvim_buf_is_valid(active_process.buffer) then
      safe_close_buffer(active_process.buffer)
    end

    -- Reset active process
    active_process = nil

    return true
  end

  return true
end

-- Function to kill the current process
local function kill_current_process(error_buf)
  if not active_process then
    vim.api.nvim_echo({ { "No active process to terminate", "WarningMsg" } }, false, {})
    return
  end

  -- Only terminate if we're in the correct buffer
  if error_buf ~= active_process.buffer then
    vim.api.nvim_echo({ { "Cannot terminate process from this buffer", "WarningMsg" } }, false, {})
    return
  end

  terminate_process(active_process)

  -- Add a message to the buffer indicating the process was killed
  if vim.api.nvim_buf_is_valid(error_buf) then
    vim.schedule(function()
      pcall(vim.api.nvim_buf_set_lines, error_buf, -1, -1, false, {
        "Process terminated by user"
      })
      vim.bo[error_buf].modifiable = false
      vim.bo[error_buf].readonly = true
    end)
  end

  active_process = nil
end

function M.run_command_async(command)
  -- Check and potentially clean up existing process
  if active_process then
    if not prompt_terminate_process() then
      return
    end
    cleanup_active_process()
  end

  -- Store the original window to return to
  local original_win = vim.api.nvim_get_current_win()

  -- Create a unique buffer name
  local buf_name = string.format('Compilation: %s', command)
  local unique_buf_name = buf_name
  local counter = 1
  while vim.fn.bufexists(unique_buf_name) ~= 0 do
    unique_buf_name = string.format('%s <%d>', buf_name, counter)
    counter = counter + 1
  end

  -- Create a new buffer at the bottom of the window
  vim.cmd('botright new')
  local error_buf = vim.api.nvim_get_current_buf()
  local error_win = vim.api.nvim_get_current_win()

  -- Rename the buffer with the unique name
  vim.cmd(string.format('file %s', unique_buf_name))

  -- Set buffer-specific options
  vim.bo.buftype = 'nofile'
  vim.bo.bufhidden = 'wipe'
  vim.bo.buflisted = false
  vim.bo.swapfile = false
  vim.bo.modifiable = true

  -- Initialize a namespace for highlights
  local ns_id = vim.api.nvim_create_namespace('CompilationHighlights')

  -- Define error link highlight
  vim.api.nvim_set_hl(0, 'ErrorLink', {
    fg = 'LightBlue',
    underline = true,
    special = 'Blue',
    ctermfg = 'Blue'
  })

  -- Function to extract error positions from the buffer
  local function get_error_positions()
    local positions = {}
    local lines = vim.api.nvim_buf_get_lines(error_buf, 0, -1, false)
    for i, line in ipairs(lines) do
      local file, lnum, col = line:match("([^:]+):(%d+):(%d+):")
      if file and lnum and col then
        table.insert(positions, {
          line = i - 1,
          file = file,
          lnum = tonumber(lnum),
          col = tonumber(col)
        })
      end
    end
    return positions
  end

  -- Function to highlight error paths in the buffer
  local function highlight_error_paths()
    vim.api.nvim_buf_clear_namespace(error_buf, ns_id, 0, -1)
    local lines = vim.api.nvim_buf_get_lines(error_buf, 0, -1, false)
    for i, line in ipairs(lines) do
      local error_pattern = "([^:]+):(%d+):(%d+):"
      local file, lnum, col = line:match(error_pattern)
      if file and lnum and col then
        -- Find the full match
        local full_match = line:match("(.*" .. file .. ":" .. lnum .. ":" .. col .. ":.*)")
        if full_match then
          local start, finish = full_match:find(file .. ":" .. lnum .. ":" .. col .. ":")
          if start and finish then
            vim.api.nvim_buf_add_highlight(error_buf, ns_id, 'ErrorLink', i - 1, start - 1, finish)
          end
        end
      end
    end
  end

  -- Append lines function with error handling
  local function append_lines(lines)
    pcall(vim.schedule, function()
      -- Additional check to ensure buffer is still valid
      if not vim.api.nvim_buf_is_valid(error_buf) then return end

      for _, line in ipairs(lines) do
        if line and line ~= "" then
          pcall(vim.api.nvim_buf_set_lines, error_buf, -1, -1, false, { line })
        end
      end

      pcall(vim.cmd, 'normal! G')
      pcall(highlight_error_paths)
    end)
  end

  -- Create pipes and spawn process
  local stdout = vim.loop.new_pipe(false)
  local stderr = vim.loop.new_pipe(false)

  local handle, pid
  handle, pid = vim.loop.spawn(vim.o.shell, {
    args = { vim.o.shellcmdflag, command },
    stdio = { nil, stdout, stderr }
  }, vim.schedule_wrap(function(code)
    -- Stop and close pipes
    stdout:read_stop()
    stderr:read_stop()
    stdout:close()
    stderr:close()
    handle:close()

    -- Schedule final buffer updates with error handling
    vim.schedule(function()
      -- Ensure buffer is still valid before updating
      if vim.api.nvim_buf_is_valid(error_buf) then
        pcall(vim.api.nvim_buf_set_lines, error_buf, -1, -1, false, {
          string.format("Process exited with code %d", code)
        })
        vim.bo.modifiable = false
        vim.bo.readonly = true
      end

      -- Clear the active process when done
      if active_process and active_process.buffer == error_buf then
        active_process = nil
      end
    end)
  end))

  -- Store active process information
  active_process = {
    buffer = error_buf,
    handle = handle,
    pid = pid
  }

  -- Read stdout and stderr with error handling
  stdout:read_start(function(err, data)
    if err then return end
    if data then
      append_lines(vim.split(data, "\n"))
    end
  end)

  stderr:read_start(function(err, data)
    if err then return end
    if data then
      append_lines(vim.split(data, "\n"))
    end
  end)

  -- Error navigation function
  local current_error_index = 0
  local function jump_to_error(direction)
    local positions = get_error_positions()
    if #positions == 0 then
      vim.api.nvim_echo({ { "No error entries found.", "WarningMsg" } }, false, {})
      return
    end

    -- Determine next/previous error
    if direction == 'next' then
      current_error_index = current_error_index + 1
      if current_error_index > #positions then
        current_error_index = 1
      end
    else
      current_error_index = current_error_index - 1
      if current_error_index < 1 then
        current_error_index = #positions
      end
    end

    local target = positions[current_error_index]

    -- Return to original window and open file
    vim.api.nvim_set_current_win(original_win)
    vim.cmd(string.format('edit +%d %s', target.lnum, target.file))
    vim.api.nvim_win_set_cursor(0, { target.lnum, target.col - 1 })

    -- Go back to error window and update cursor
    vim.api.nvim_set_current_win(error_win)
    vim.api.nvim_win_set_cursor(error_win, { target.line + 1, 0 })
  end

  -- Key mappings
  vim.keymap.set('n', 'n', function() jump_to_error('next') end, { buffer = true })
  vim.keymap.set('n', 'N', function() jump_to_error('prev') end, { buffer = true })
  -- Update Ctrl-C mapping to pass the current buffer
  vim.keymap.set({ 'n', 'i' }, '<C-c>', function() kill_current_process(error_buf) end, { buffer = true })

  -- Enter key to open file in original window
  vim.keymap.set('n', '<CR>', function()
    local line = vim.fn.getline('.')
    local file, lnum, col = line:match("([^:]+):(%d+):(%d+):")
    if file and lnum and col then
      vim.api.nvim_set_current_win(original_win)
      vim.cmd(string.format('edit +%d %s', lnum, file))
      vim.api.nvim_win_set_cursor(0, { tonumber(lnum), tonumber(col) - 1 })

      -- Return focus to error window
      vim.api.nvim_set_current_win(error_win)
    else
      vim.api.nvim_echo({ { "No error entry on this line.", "WarningMsg" } }, false, {})
    end
  end, { buffer = true })

  vim.keymap.set('n', 'q', ':close<CR>', { buffer = true })

  -- Initial highlighting
  highlight_error_paths()
end

function M.prompt_and_run_command_async()
  local command = vim.fn.input({
    prompt = 'Enter command: ',
    completion = 'shellcmd' -- Enable shell command completion
  })

  -- Check if command is not empty (user didn't press Esc or enter empty string)
  if command and command:match("%S") then
    M.run_command_async(command)
  end
end

return M
