-- Leader keymap mirroring Neovim (<leader> = Space) and Doom (SPC).
-- A bare Space can't be a terminal leader, so ctrl+space stands in for it;
-- pressing it twice sends a real ctrl+space to the program.
local L = "ctrl+space>"
local dirs = { h = "left", j = "down", k = "up", l = "right" }

-- Minimizing takes a pane out of the layout; it keeps running, detached.
-- Maximizing puts one back beside the focused pane, picking from a list when
-- more than one is hidden.
local function detached(session_id)
  local result = rex.call("session.list_blocks", { session_id = session_id }) or {}
  local blocks = {}
  for _, block in ipairs(result.blocks or {}) do
    if block.placement == "detached" then
      blocks[#blocks + 1] = block
    end
  end
  return blocks
end

local function find_rect(node, block_id)
  if not node then
    return nil
  end
  if node.block_id == block_id then
    return node.rect
  end
  if node.split then
    return find_rect(node.split.before, block_id) or find_rect(node.split.after, block_id)
  end
end

local function restore(ctx, block_id)
  local view = rex.call("session.view", { session_id = ctx.session_id }) or {}
  local rect
  for _, window in ipairs(view.windows or {}) do
    for _, layer in ipairs(window.layers or {}) do
      rect = rect or find_rect(layer.layout, ctx.block_id)
    end
  end
  -- Split the anchor's longer side, like pane.split.auto. Rects are fractions
  -- of the window, which is assumed to be roughly 16:10.
  local wide = not rect or rect[3] * 1.6 >= rect[4]
  rex.call("session.move_block", {
    session_id = ctx.session_id,
    block_id = block_id,
    anchor_block_id = ctx.block_id,
    direction = wide and "horizontal" or "vertical",
  })
  rex.call("session.focus_block", { session_id = ctx.session_id, block_id = block_id })
end

rex.action({
  name = "pane_minimize",
  title = "Minimize Pane",
  run = function(ctx)
    rex.call("session.detach_block", { session_id = ctx.session_id, block_id = ctx.block_id })
  end,
})

rex.action({
  name = "pane_restore",
  title = "Restore Minimized Pane",
  run = function(ctx)
    local blocks = detached(ctx.session_id)
    if #blocks == 0 then
      return
    elseif #blocks == 1 then
      return restore(ctx, blocks[1].block_id)
    end
    local items = {}
    for _, block in ipairs(blocks) do
      local info = rex.call("block.method", {
        session_id = ctx.session_id,
        block_id = block.block_id,
        method = "title",
      }) or {}
      items[#items + 1] = {
        id = block.block_id,
        title = info.title or block.label,
        subtitle = info.owner and info.owner.name or nil,
      }
    end
    local choice = rex.ui.pick({ title = "Restore Pane", placeholder = "Minimized panes", items = items })
    if type(choice) == "table" then
      choice = choice.id
    end
    if choice then
      restore(ctx, choice)
    end
  end,
})

rex.bind(L .. "ctrl+space", "pane.send_key", { key = "ctrl+space" })

rex.bind(L .. "space", "client.palette.toggle") -- M-x / <leader><leader>
rex.bind(L .. "s", "pane.split.auto") -- same as cmd+enter: best-fit split
rex.bind(L .. "z", "pane.zoom")
rex.bind(L .. "m", "pane_minimize")
rex.bind(L .. "shift+m", "pane_restore")
rex.bind(L .. "q", "pane.close")
rex.bind(L .. "t", "client.tab.new") -- Doom SPC t
rex.bind(L .. "n", "client.tab.next")
rex.bind(L .. "p", "client.tab.previous")
rex.bind(L .. "f", "pane.go_to") -- files
rex.bind(L .. "o", "pane.go_to_directory") -- Doom SPC o (dired)
rex.bind(L .. "b", "session.switch") -- buffers
rex.bind(L .. "v", "client.find.open") -- Doom SPC v (search buffer)
rex.bind(L .. "shift+r", "client.config.reload")
for key, direction in pairs(dirs) do
  rex.bind(L .. key, "pane.focus", { direction = direction })
end
for i = 1, 9 do
  rex.bind(L .. i, "client.tab.goto", { index = i })
end

-- Sticky window mode, like Doom's SPC w / Evil's C-w: hjkl focus, HJKL move,
-- ctrl+hjkl resize, s split, = balance, z zoom, q close. Escape leaves;
-- other keys are swallowed rather than typed into the pane.
rex.mode("window", { exclusive = true })
rex.bind(L .. "w", "client.mode.enter", { name = "window" })
for key, direction in pairs(dirs) do
  rex.bind("window/" .. key, "pane.focus", { direction = direction })
  rex.bind("window/shift+" .. key, "pane.move", { direction = direction })
  rex.bind("window/ctrl+" .. key, "pane.resize", { direction = direction })
end
rex.bind("window/s", "pane.split.auto")
rex.bind("window/=", "pane.balance")
rex.bind("window/z", "pane.zoom")
rex.bind("window/q", "pane.close")
rex.bind("window/escape", "client.mode.exit")
