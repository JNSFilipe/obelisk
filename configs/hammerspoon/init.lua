local emacsMap = hs.hotkey.modal.new()

hs.window.animationDuration = 0

local obeliskState = rawget(_G, "obeliskState") or {}
_G.obeliskState = obeliskState
obeliskState.menubars = obeliskState.menubars or {}

local ctrlXActive = false
local markActive = false
local currentAppName = nil
local currentBundleID = nil

local excludedBundleIDs = {
  ["org.gnu.Emacs"] = true,
  ["org.gnu.AquamacsEmacs"] = true,
  ["org.gnu.Aquamacs"] = true,
  ["com.apple.Terminal"] = true,
  ["com.googlecode.iterm2"] = true,
  ["com.github.wez.wezterm"] = true,
  ["net.kovidgoyal.kitty"] = true,
  ["com.mitchellh.ghostty"] = true,
  ["com.cmuxterm.app"] = true,
  ["dev.zed.Zed"] = true,
  ["com.microsoft.VSCode"] = true,
  ["com.qvacua.VimR"] = true,
  ["com.utmapp.UTM"] = true,
}

local excludedBundleIDPatterns = {
  "^org%.vim%.",
  "^com%.jetbrains%.",
  "^com%.sublimetext%.",
  "^com%.microsoft%.rdc",
  "^com%.teamviewer%.TeamViewer$",
  "^com%.vmware%.",
  "^com%.parallels%.",
  "^com%.citrix%.",
}

local excludedAppNames = {
  ["emacs"] = true,
  ["terminal"] = true,
  ["wezterm"] = true,
  ["ghostty"] = true,
  ["cmux"] = true,
  ["zed"] = true,
  ["visual studio code"] = true,
  ["datagrip"] = true,
  ["sublime text"] = true,
}

local keys = {
  ctrl = {
    a = {{"ctrl"}, "a", true},
    b = {nil, "left", true},
    d = {{"ctrl"}, "d", false},
    e = {{"ctrl"}, "e", true},
    f = {nil, "right", true},
    g = {nil, "escape", false, function() markActive = false end},
    k = {{"ctrl"}, "k", false},
    n = {nil, "down", true},
    o = {nil, "return", false},
    p = {nil, "up", true},
    r = {{"cmd"}, "f", false},
    s = {{"cmd"}, "f", false},
    v = {nil, "pagedown", true},
    w = {{"cmd"}, "x", false},
    x = {nil, nil, false, function() startCtrlX() end},
    y = {{"cmd"}, "v", false},
    ["/"] = {{"cmd"}, "z", false},
    space = {nil, nil, true, function() markActive = not markActive end},
  },
  ctrlX = {
    b = {{"cmd", "shift"}, "a", false},
    c = {{"cmd"}, "q", false},
    f = {{"cmd"}, "o", false},
    h = {{"cmd"}, "a", false},
    k = {{"cmd"}, "w", false},
    n = {{"cmd"}, "n", false},
    o = {{"cmd"}, "`", false},
    p = {{"cmd", "alt"}, "f", false},
    q = {{"cmd"}, "q", false},
    r = {{"cmd"}, "r", false},
    s = {{"cmd"}, "s", false},
    u = {{"cmd"}, "z", false},
    w = {{"cmd"}, "s", false},
    ["7"] = {{"cmd", "shift"}, "7", false},
    ["8"] = {{"cmd", "shift"}, "8", false},
  },
  alt = {
    b = {{"alt"}, "left", true},
    d = {{"alt"}, "forwarddelete", false},
    f = {{"alt"}, "right", true},
    v = {nil, "pageup", true},
    w = {{"cmd"}, "c", false},
    y = {{"cmd"}, "v", false},
  },
  altShift = {
    [","] = {nil, "home", false},
    ["."] = {nil, "end", false},
  },
}

local function focusedWindow()
  local win = hs.window.focusedWindow()
  if win == nil then
    hs.alert.show("No focused window", 0.6)
  end
  return win
end

local function setFocusedWindow(unit)
  return function()
    local win = focusedWindow()
    if win == nil then
      return
    end

    local frame = win:screen():frame()
    win:setFrame({
      x = frame.x + frame.w * unit.x,
      y = frame.y + frame.h * unit.y,
      w = frame.w * unit.w,
      h = frame.h * unit.h,
    }, 0)
  end
end

local function resizeFocusedWindow(delta)
  return function()
    local win = focusedWindow()
    if win == nil then
      return
    end

    local screenFrame = win:screen():frame()
    local frame = win:frame()
    local centerX = frame.x + frame.w / 2
    local centerY = frame.y + frame.h / 2
    local width = math.max(math.floor(screenFrame.w * 0.35), math.min(screenFrame.w, math.floor(frame.w + screenFrame.w * delta)))
    local height = math.max(math.floor(screenFrame.h * 0.35), math.min(screenFrame.h, math.floor(frame.h + screenFrame.h * delta)))

    win:setFrame({
      x = math.max(screenFrame.x, math.min(screenFrame.x + screenFrame.w - width, math.floor(centerX - width / 2))),
      y = math.max(screenFrame.y, math.min(screenFrame.y + screenFrame.h - height, math.floor(centerY - height / 2))),
      w = width,
      h = height,
    }, 0)
  end
end

local windowBindings = {
  {key = "h", fn = setFocusedWindow({x = 0, y = 0, w = 0.5, h = 1})},
  {key = "j", fn = setFocusedWindow({x = 0, y = 0.5, w = 1, h = 0.5})},
  {key = "k", fn = setFocusedWindow({x = 0, y = 0, w = 1, h = 0.5})},
  {key = "l", fn = setFocusedWindow({x = 0.5, y = 0, w = 0.5, h = 1})},
  {key = "m", fn = setFocusedWindow({x = 0, y = 0, w = 1, h = 1})},
  {key = "left", fn = resizeFocusedWindow(-0.10)},
  {key = "right", fn = resizeFocusedWindow(0.10)},
}

local caffeineMenu = obeliskState.menubars.caffeine
if caffeineMenu == nil then
  caffeineMenu = hs.menubar.new()
  obeliskState.menubars.caffeine = caffeineMenu
end

local function caffeineIcon(awake)
  local color = {white = awake and 1.0 or 0.55, alpha = 1}
  local elements = {
    {
      type = "rectangle",
      action = "stroke",
      strokeWidth = 1.8,
      strokeColor = color,
      roundedRectRadii = {xRadius = 2.5, yRadius = 2.5},
      frame = {x = 5, y = 10, w = 11, h = 7},
    },
    {
      type = "oval",
      action = "stroke",
      strokeWidth = 1.8,
      strokeColor = color,
      frame = {x = 14, y = 11, w = 5, h = 5},
    },
    {
      type = "segments",
      action = "stroke",
      strokeWidth = 1.8,
      strokeColor = color,
      closed = false,
      coordinates = {
        {x = 4, y = 18},
        {x = 18, y = 18},
      },
    },
  }

  if awake then
    table.insert(elements, {
      type = "segments",
      action = "stroke",
      strokeWidth = 1.5,
      strokeColor = color,
      closed = false,
      coordinates = {
        {x = 8, y = 8},
        {x = 7, y = 6},
        {x = 8, y = 4},
        {x = 7, y = 2},
      },
    })
    table.insert(elements, {
      type = "segments",
      action = "stroke",
      strokeWidth = 1.5,
      strokeColor = color,
      closed = false,
      coordinates = {
        {x = 12, y = 8},
        {x = 13, y = 6},
        {x = 12, y = 4},
        {x = 13, y = 2},
      },
    })
  else
    table.insert(elements, {
      type = "segments",
      action = "stroke",
      strokeWidth = 1.4,
      strokeColor = color,
      closed = false,
      coordinates = {
        {x = 6, y = 8},
        {x = 15, y = 8},
      },
    })
  end

  local canvas = hs.canvas.new({x = 0, y = 0, w = 22, h = 22})
  if canvas == nil then
    return nil
  end

  local ok, image = pcall(function()
    canvas:replaceElements(elements)
    return canvas:imageFromCanvas()
  end)
  canvas:delete()

  if ok then
    return image
  end

  return nil
end

local caffeineIcons = {
  idle = caffeineIcon(false),
  awake = caffeineIcon(true),
}

local function setMenuIconOrTitle(menu, icon, fallbackTitle)
  menu:setTitle(fallbackTitle)

  if icon == nil then
    return
  end

  local ok = pcall(function()
    menu:setIcon(icon, true)
    menu:setTitle("")
  end)

  if not ok then
    menu:setIcon(nil)
    menu:setTitle(fallbackTitle)
  end
end

local function caffeineEnabled()
  return hs.caffeinate.get("displayIdle") or hs.caffeinate.get("systemIdle")
end

local function updateCaffeineMenu()
  if caffeineMenu == nil then
    return
  end

  if caffeineEnabled() then
    setMenuIconOrTitle(caffeineMenu, caffeineIcons.awake, "Coffee")
    caffeineMenu:setTooltip("Caffeinate is on")
  else
    setMenuIconOrTitle(caffeineMenu, caffeineIcons.idle, "Sleep")
    caffeineMenu:setTooltip("Caffeinate is off")
  end
end

local function setCaffeinate(enabled)
  hs.caffeinate.set("displayIdle", enabled)
  hs.caffeinate.set("systemIdle", enabled)
  updateCaffeineMenu()
end

local function toggleCaffeinate()
  local enabled = not caffeineEnabled()
  setCaffeinate(enabled)
  hs.alert.show(enabled and "Caffeinate on" or "Caffeinate off", 0.7)
end

if caffeineMenu ~= nil then
  caffeineMenu:setClickCallback(toggleCaffeinate)
  updateCaffeineMenu()
end

for _, binding in ipairs(windowBindings) do
  hs.hotkey.bind({"cmd", "ctrl"}, binding.key, binding.fn)
end

hs.hotkey.bind({"cmd", "ctrl"}, "c", toggleCaffeinate)

local leaderModal = hs.hotkey.modal.new({"cmd", "ctrl"}, "a")
local leaderCanvas = nil

local leaderItems = {
  {key = "b", label = "Firefox", bundleID = "org.mozilla.firefox", path = "/Applications/Firefox.app"},
  {key = "c", label = "Caffeinate", detail = "Toggle keep-awake", action = "caffeinate"},
  {key = "n", label = "Notion", bundleID = "notion.id", path = "/Applications/Notion.app"},
  {key = "C", mods = {"shift"}, bind = "c", label = "Notion Calendar", bundleID = "com.cron.electron", path = "/Applications/Notion Calendar.app"},
  {key = "d", label = "DataGrip", bundleID = "com.jetbrains.datagrip", path = "/Applications/DataGrip.app"},
  {key = "e", label = "Emacs", bundleID = "org.gnu.Emacs", path = "/Applications/Emacs.app"},
  {key = "f", label = "Finder", bundleID = "com.apple.finder", path = "/System/Library/CoreServices/Finder.app", action = "openBundle"},
  {key = "g", label = "GitHub Desktop", bundleID = "com.github.GitHubClient", path = "/Applications/GitHub Desktop.app"},
  {key = "m", label = "Mail", bundleID = "com.apple.mail", path = "/System/Applications/Mail.app", action = "openBundle"},
  {key = "p", label = "Postman", bundleID = "com.postmanlabs.mac", path = "/Applications/Postman.app"},
  {key = "s", label = "Sublime Text", bundleID = "com.sublimetext.4", path = "/Applications/Sublime Text.app"},
  {key = "T", mods = {"shift"}, bind = "t", label = "WezTerm", bundleID = "com.github.wez.wezterm", path = "/Applications/WezTerm.app"},
  {key = "t", label = "Microsoft Teams", bundleID = "com.microsoft.teams2", path = "/Applications/Microsoft Teams.app"},
  {key = "v", label = "Visual Studio Code", bundleID = "com.microsoft.VSCode", path = "/Applications/Visual Studio Code.app"},
  {key = "w", label = "WhatsApp", bundleID = "net.whatsapp.WhatsApp", path = "/Applications/WhatsApp.app"},
  {key = "y", label = "Spotify", bundleID = "com.spotify.client", path = "/Applications/Spotify.app"},
}

local leaderItemsByKey = {}

for _, item in ipairs(leaderItems) do
  leaderItemsByKey[item.key] = item
end

local function hideLeaderCanvas()
  if leaderCanvas ~= nil then
    leaderCanvas:hide()
  end
end

local function dismissLeader()
  hideLeaderCanvas()
end

local function leaderKeyLabel(item)
  if item.key == "space" then
    return "SPC"
  end
  return item.key
end

local function leaderLayout()
  local screenFrame = hs.screen.mainScreen():frame()
  local columns = 2
  local rows = math.ceil(#leaderItems / columns)
  local margin = 28
  local gutter = 24
  local headerHeight = 58
  local footerHeight = 36
  local rowHeight = 52
  local width = math.min(1120, math.floor(screenFrame.w * 0.90))
  local height = headerHeight + footerHeight + margin + rows * rowHeight
  local x = screenFrame.x + math.floor((screenFrame.w - width) / 2)
  local y = screenFrame.y + math.floor((screenFrame.h - height) / 2)

  return {
    x = x,
    y = y,
    w = width,
    h = height,
    columns = columns,
    rows = rows,
    margin = margin,
    gutter = gutter,
    headerHeight = headerHeight,
    footerHeight = footerHeight,
    rowHeight = rowHeight,
    columnWidth = math.floor((width - margin * 2 - gutter) / columns),
  }
end

local function leaderTextElement(text, frame, size, color, font, alignment)
  return {
    type = "text",
    text = text,
    textFont = font or ".AppleSystemUIFont",
    textSize = size,
    textColor = color,
    textAlignment = alignment or "left",
    frame = frame,
  }
end

local function leaderIcon(path)
  if path == nil or hs.image.iconForFile == nil then
    return nil
  end
  return hs.image.iconForFile(path)
end

local function activateRunningApplication(bundleID)
  if bundleID == nil then
    return false
  end

  local app = hs.application.get(bundleID)
  if app == nil and hs.application.applicationsForBundleID ~= nil then
    for _, candidate in ipairs(hs.application.applicationsForBundleID(bundleID) or {}) do
      app = candidate
      break
    end
  end

  if app ~= nil then
    local window = app:mainWindow()
    if window ~= nil then
      window:focus()
      return true
    end

    app:activate()
    return true
  end

  return false
end

local function openBundle(bundleID)
  if bundleID == nil then
    return false
  end

  local task = hs.task.new("/usr/bin/open", nil, {"-b", bundleID})
  if task == nil then
    return false
  end

  task:start()
  return true
end

local function openApplication(item)
  if item.action == "caffeinate" then
    toggleCaffeinate()
    return
  end

  if item.action == "openBundle" and openBundle(item.bundleID) then
    return
  end

  if activateRunningApplication(item.bundleID) then
    return
  end

  if item.bundleID ~= nil and hs.application.launchOrFocusByBundleID(item.bundleID) then
    return
  end

  if hs.application.open ~= nil and hs.application.open(item.path) then
    return
  end

  if item.bundleID ~= nil and openBundle(item.bundleID) then
    return
  end

  local task = hs.task.new("/usr/bin/open", nil, {item.path})
  if task ~= nil then
    task:start()
  end
end

local function createLeaderCanvas()
  local layout = leaderLayout()
  local elements = {
    {
      type = "rectangle",
      action = "fill",
      fillColor = {white = 0.07, alpha = 0.96},
      roundedRectRadii = {xRadius = 16, yRadius = 16},
      frame = {x = 0, y = 0, w = layout.w, h = layout.h},
    },
    {
      type = "rectangle",
      action = "stroke",
      strokeWidth = 1,
      strokeColor = {white = 0.32, alpha = 0.90},
      roundedRectRadii = {xRadius = 16, yRadius = 16},
      frame = {x = 0.5, y = 0.5, w = layout.w - 1, h = layout.h - 1},
    },
    leaderTextElement(
      "Leader",
      {x = layout.margin, y = 18, w = 180, h = 28},
      22,
      {white = 0.95, alpha = 1},
      ".AppleSystemUIFontSemibold"
    ),
    leaderTextElement(
      "esc cancel",
      {x = layout.w - layout.margin - 130, y = 22, w = 130, h = 22},
      14,
      {white = 0.58, alpha = 1},
      ".AppleSystemUIFont",
      "right"
    ),
  }

  for index, item in ipairs(leaderItems) do
    local column = math.floor((index - 1) / layout.rows)
    local row = (index - 1) % layout.rows
    local x = layout.margin + column * (layout.columnWidth + layout.gutter)
    local y = layout.headerHeight + row * layout.rowHeight
    local icon = leaderIcon(item.path)
    local textX = x + 62
    local textWidth = layout.columnWidth - 62

    if icon ~= nil then
      textX = x + 104
      textWidth = layout.columnWidth - 104
    end

    table.insert(elements, {
      type = "rectangle",
      action = "fill",
      fillColor = {white = 0.13, alpha = 0.84},
      roundedRectRadii = {xRadius = 7, yRadius = 7},
      frame = {x = x, y = y + 8, w = 46, h = 34},
    })
    table.insert(elements, leaderTextElement(
      leaderKeyLabel(item),
      {x = x, y = y + 12, w = 46, h = 24},
      17,
      {white = 0.93, alpha = 1},
      ".AppleSystemUIFontSemibold",
      "center"
    ))
    if icon ~= nil then
      table.insert(elements, {
        type = "image",
        image = icon,
        imageScaling = "scaleProportionally",
        frame = {x = x + 58, y = y + 10, w = 30, h = 30},
      })
    end
    table.insert(elements, leaderTextElement(
      item.label,
      {x = textX, y = y + 5, w = textWidth, h = 22},
      19,
      {white = 0.94, alpha = 1},
      ".AppleSystemUIFontSemibold"
    ))
    table.insert(elements, leaderTextElement(
      item.detail or item.path or "",
      {x = textX, y = y + 28, w = textWidth, h = 18},
      13,
      {white = 0.58, alpha = 1},
      ".AppleSystemUIFont"
    ))
  end

  local canvas = hs.canvas.new({x = layout.x, y = layout.y, w = layout.w, h = layout.h})
  canvas:level(hs.canvas.windowLevels.floating)
  canvas:replaceElements(elements)
  return canvas
end

local function showLeader()
  if leaderCanvas == nil then
    leaderCanvas = createLeaderCanvas()
  end

  leaderCanvas:show()
end

local function exitLeader()
  dismissLeader()
  leaderModal:exit()
end

local function runLeaderItem(item)
  hideLeaderCanvas()
  leaderModal:exit()
  hs.timer.doAfter(0, function()
    openApplication(item)
  end)
end

local function handleLeaderKey(key)
  return function()
    local item = leaderItemsByKey[key]
    if item ~= nil then
      runLeaderItem(item)
      return
    end
    hs.alert.show("No leader binding: " .. key, 0.5)
  end
end

leaderModal.entered = function()
  showLeader()
end

leaderModal.exited = function()
  dismissLeader()
  if shouldEnableForCurrentApp() then
    emacsMap:enter()
  end
end

leaderModal:bind({}, "escape", exitLeader)

for _, item in ipairs(leaderItems) do
  leaderModal:bind(item.mods or {}, item.bind or item.key, handleLeaderKey(item.key))
end

local function tableContains(list, value)
  for _, item in ipairs(list) do
    if item == value then
      return true
    end
  end
  return false
end

local function withShift(mods)
  local result = {}
  for _, mod in ipairs(mods or {}) do
    table.insert(result, mod)
  end
  if not tableContains(result, "shift") then
    table.insert(result, "shift")
  end
  return result
end

local function tapKey(mods, key)
  if key == nil then
    return
  end
  hs.eventtap.event.newKeyEvent(mods or {}, key, true):post()
  hs.eventtap.event.newKeyEvent(mods or {}, key, false):post()
end

function startCtrlX()
  ctrlXActive = true
  hs.timer.doAfter(2.0, function()
    ctrlXActive = false
  end)
end

local function runBinding(binding)
  local mods = binding[1]
  local key = binding[2]
  local markSensitive = binding[3]
  local macro = binding[4]

  if macro ~= nil then
    macro()
  else
    if markActive and markSensitive then
      tapKey(withShift(mods), key)
    else
      tapKey(mods, key)
    end
  end

  if not markSensitive then
    markActive = false
  end
end

local function processKey(mod, key)
  return function()
    emacsMap:exit()

    local namespace = mod
    if ctrlXActive and mod == "ctrl" then
      namespace = "ctrlX"
    end

    local binding = keys[namespace] and keys[namespace][key]
    if binding ~= nil then
      runBinding(binding)
    else
      tapKey({mod}, key)
    end

    if namespace ~= "ctrl" or key ~= "x" then
      ctrlXActive = false
    end

    if shouldEnableForCurrentApp() then
      emacsMap:enter()
    end
  end
end

local function bindKeys()
  local letters = {
    "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m",
    "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z",
  }

  for _, letter in ipairs(letters) do
    emacsMap:bind({"ctrl"}, letter, processKey("ctrl", letter), nil)
    emacsMap:bind({"alt"}, letter, processKey("alt", letter), nil)
  end

  emacsMap:bind({"cmd", "ctrl"}, "a", function()
    emacsMap:exit()
    leaderModal:enter()
  end, nil)

  emacsMap:bind({"ctrl"}, "space", processKey("ctrl", "space"), nil)
  emacsMap:bind({"ctrl"}, "/", processKey("ctrl", "/"), nil)
  emacsMap:bind({"ctrl"}, "7", processKey("ctrl", "7"), nil)
  emacsMap:bind({"ctrl"}, "8", processKey("ctrl", "8"), nil)
  emacsMap:bind({"alt", "shift"}, ",", processKey("altShift", ","), nil)
  emacsMap:bind({"alt", "shift"}, ".", processKey("altShift", "."), nil)

end

local function bundleIsExcluded(bundleID)
  if bundleID == nil then
    return false
  end
  if excludedBundleIDs[bundleID] then
    return true
  end
  for _, pattern in ipairs(excludedBundleIDPatterns) do
    if string.match(bundleID, pattern) then
      return true
    end
  end
  return false
end

function shouldEnableForCurrentApp()
  if bundleIsExcluded(currentBundleID) then
    return false
  end
  if currentAppName ~= nil and excludedAppNames[string.lower(currentAppName)] then
    return false
  end
  return true
end

local function updateCurrentApp(app)
  currentAppName = nil
  currentBundleID = nil

  if app ~= nil then
    currentAppName = app:name()
    currentBundleID = app:bundleID()
  end

  ctrlXActive = false
  markActive = false

  if shouldEnableForCurrentApp() then
    emacsMap:enter()
  else
    emacsMap:exit()
  end
end

bindKeys()
updateCurrentApp(hs.application.frontmostApplication())

emacsAppWatcher = hs.application.watcher.new(function(_, eventType, app)
  if eventType == hs.application.watcher.activated then
    updateCurrentApp(app)
  end
end)

emacsAppWatcher:start()
