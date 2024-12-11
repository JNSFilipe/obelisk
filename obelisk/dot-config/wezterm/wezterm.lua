local wezterm = require("wezterm")
local config = {}

-- Set theme
-- config.color_scheme = "Catppuccin Mocha"
-- config.color_scheme = "Oxocarbon Dark"
config.color_scheme = "tokyonight_night"
config.window_background_opacity = 0.9

-- Config Fonts
config.font = wezterm.font("JetBrainsMono Nerd Font")
config.font_size = 12

-- Config UI elements
config.hide_tab_bar_if_only_one_tab = true
config.use_fancy_tab_bar = false
config.window_padding = { left = 3, right = 0, top = 3, bottom = 0 }

-- Set zsh as starting program
config.default_prog = { "zsh" }

-- Circunvent bug, reported in https://github.com/wez/wezterm/issues/4483#issuecomment-1835619115
config.enable_wayland = false

-- Hide titlebar
config.window_decorations = "RESIZE"

-- Make both option keys work on mac
-- https://github.com/wez/wezterm/issues/5468#issuecomment-2133721206
config.send_composed_key_when_left_alt_is_pressed = true
config.send_composed_key_when_right_alt_is_pressed = true

-- Blazingly fast!
-- https://www.reddit.com/r/neovim/comments/1gthknw/wezterm_max_fps_240_is_crazy/
config.max_fps = 240

return config
