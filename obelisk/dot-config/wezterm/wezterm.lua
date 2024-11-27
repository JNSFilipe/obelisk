local wezterm = require("wezterm")
local config = {}

-- Set theme
-- config.color_scheme = "Catppuccin Mocha"
-- config.color_scheme = "Oxocarbon Dark"
config.color_scheme = "tokyonight_night"
config.window_background_opacity = 0.9

-- Config Fonts
config.font = wezterm.font("JetBrainsMono Nerd Font")
config.font_size = 11

-- Config UI elements
config.hide_tab_bar_if_only_one_tab = true
config.use_fancy_tab_bar = false
config.window_padding = { left = 3, right = 0, top = 3, bottom = 0 }

-- Set zsh as starting program
config.default_prog = { "zsh" }

-- Circunvent bug, reported in https://github.com/wez/wezterm/issues/4483#issuecomment-1835619115
config.enable_wayland = false

return config
