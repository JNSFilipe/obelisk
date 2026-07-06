local emacsMap = hs.hotkey.modal.new()

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
    ["1"] = {{"cmd", "shift"}, "w", false},
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

  emacsMap:bind({"ctrl"}, "space", processKey("ctrl", "space"), nil)
  emacsMap:bind({"ctrl"}, "/", processKey("ctrl", "/"), nil)
  emacsMap:bind({"ctrl"}, "1", processKey("ctrl", "1"), nil)
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
