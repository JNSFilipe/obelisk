local wezterm = require("wezterm")
local sessionizer = require("sessionizer")
local config = {}
local act = wezterm.action

local palette = {
	bg = "#0f111a",
	surface = "#16161e",
	surface_alt = "#1f2335",
	text = "#c0caf5",
	muted = "#565f89",
	accent = "#7aa2f7",
	success = "#9ece6a",
	active_badge_bg = "#bb9af7",
	active_badge_fg = "#1a1b26",
	inactive_badge_bg = "#2f334d",
	inactive_badge_fg = "#a9b1d6",
}
local rounded_left = ""
local rounded_right = ""
local pill_gap = " "
local workspace_icon = " "

local function tab_title(tab_info)
	local title = tab_info.tab_title
	if title and #title > 0 then
		return title
	end
	return tab_info.active_pane.title
end

wezterm.on("sessionizer", function(window, pane)
	sessionizer.toggle(window, pane)
end)

wezterm.on("sessionizer-switch", function(window, pane)
	sessionizer.switch_session(window, pane)
end)

wezterm.on("update-right-status", function(window, _pane)
	local workspace = window:active_workspace()
	local left_status = wezterm.format({
		{ Background = { Color = palette.bg } },
		{ Text = pill_gap },
		{ Foreground = { Color = palette.success } },
		{ Background = { Color = palette.bg } },
		{ Text = rounded_left },
		{ Background = { Color = palette.success } },
		{ Foreground = { Color = palette.bg } },
		{ Text = " " .. workspace_icon .. " " },
		{ Background = { Color = palette.surface_alt } },
		{ Foreground = { Color = palette.text } },
		{ Text = " " .. workspace .. " " },
		{ Foreground = { Color = palette.surface_alt } },
		{ Background = { Color = palette.bg } },
		{ Text = rounded_right },
		{ Background = { Color = palette.bg } },
		{ Text = pill_gap },
	})
	window:set_left_status(left_status)
end)

wezterm.on("format-tab-title", function(tab, _tabs, _panes, _cfg, _hover, max_width)
	local title = tab_title(tab)
	local title_width = math.max(8, max_width - 7)
	if #title > title_width then
		title = wezterm.truncate_right(title, title_width)
	end

	local index_bg = palette.inactive_badge_bg
	local index_fg = palette.inactive_badge_fg
	local tab_bg = palette.surface
	local tab_fg = palette.muted

	if tab.is_active then
		index_bg = palette.active_badge_bg
		index_fg = palette.active_badge_fg
		tab_bg = palette.surface_alt
		tab_fg = palette.text
	end

	return {
		{ Background = { Color = palette.bg } },
		{ Text = pill_gap },
		{ Foreground = { Color = index_bg } },
		{ Background = { Color = palette.bg } },
		{ Text = rounded_left },
		{ Background = { Color = index_bg } },
		{ Foreground = { Color = index_fg } },
		{ Text = " " .. (tab.tab_index + 1) .. " " },
		{ Background = { Color = tab_bg } },
		{ Foreground = { Color = tab_fg } },
		{ Text = " " .. title .. " " },
		{ Foreground = { Color = tab_bg } },
		{ Background = { Color = palette.bg } },
		{ Text = rounded_right },
		{ Background = { Color = palette.bg } },
		{ Text = pill_gap },
	}
end)

-- Set theme
-- config.color_scheme = "Catppuccin Mocha"
-- config.color_scheme = "Oxocarbon Dark"
config.color_scheme = "tokyonight_night"
-- config.window_background_opacity = 0.9

-- Config Fonts
-- config.font = wezterm.font("JetBrainsMono Nerd Font")
config.font = wezterm.font("Iosevka")
config.font_size = 13 -- For Iosevka, 13 is the default size. For JetBrains Mono, 12 is the default size.
-- config.font_size = 12

-- Config UI elements
config.hide_tab_bar_if_only_one_tab = false
config.use_fancy_tab_bar = false
config.show_new_tab_button_in_tab_bar = false
config.tab_max_width = 56
config.window_padding = { left = 3, right = 0, top = 3, bottom = 0 }
config.colors = {
	tab_bar = {
		background = palette.bg,
		active_tab = {
			bg_color = palette.surface_alt,
			fg_color = palette.text,
		},
		inactive_tab = {
			bg_color = palette.surface,
			fg_color = palette.muted,
		},
		inactive_tab_hover = {
			bg_color = palette.surface_alt,
			fg_color = palette.text,
		},
		new_tab = {
			bg_color = palette.bg,
			fg_color = palette.muted,
		},
		new_tab_hover = {
			bg_color = palette.surface_alt,
			fg_color = palette.text,
		},
	},
}

-- Set zsh as starting program
config.default_prog = { "zsh" }

-- Circumvent bug, reported in https://github.com/wez/wezterm/issues/4483#issuecomment-1835619115
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

config.leader = { key = "z", mods = "CTRL", timeout_milliseconds = 2000 }
config.keys = {
	{ key = "Space", mods = "LEADER", action = act.EmitEvent("sessionizer") },
	{ key = "z", mods = "LEADER|CTRL", action = act.EmitEvent("sessionizer-switch") },
}

return config
