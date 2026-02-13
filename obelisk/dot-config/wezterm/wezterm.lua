local wezterm = require("wezterm")
local sessionizer = require("sessionizer")
local config = {}
local act = wezterm.action

-- Set theme
-- local color_scheme_name = "Catppuccin Mocha"
-- local color_scheme_name = "Oxocarbon Dark"
local color_scheme_name = "oxocarbon"
config.color_scheme = color_scheme_name
local custom_schemes = {}
local ok, loaded_oxocarbon = pcall(function()
	return wezterm.color.load_scheme(wezterm.config_dir .. "/colors/oxocarbon.toml")
end)
if ok and loaded_oxocarbon then
	custom_schemes.oxocarbon = loaded_oxocarbon
	config.color_schemes = custom_schemes
end

local function first_non_nil(...)
	for i = 1, select("#", ...) do
		local value = select(i, ...)
		if value ~= nil then
			return value
		end
	end
	return nil
end

local function palette_from_scheme(scheme_name)
	local builtins = wezterm.get_builtin_color_schemes()
	local scheme = custom_schemes[scheme_name] or builtins[scheme_name] or {}
	local tab_bar = scheme.tab_bar or {}
	local active_tab = tab_bar.active_tab or {}
	local inactive_tab = tab_bar.inactive_tab or {}
	local ansi = scheme.ansi or {}
	local brights = scheme.brights or {}

	return {
		bg = first_non_nil(tab_bar.background, scheme.background, "#000000"),
		-- Keep base surfaces neutral (gray-ish) instead of theme accent colors.
		surface = first_non_nil(ansi[1], inactive_tab.bg_color, scheme.background, "#111111"),
		surface_alt = first_non_nil(brights[1], ansi[1], inactive_tab.bg_color, scheme.background, "#222222"),
		text = first_non_nil(scheme.foreground, active_tab.fg_color, "#ffffff"),
		muted = first_non_nil(inactive_tab.fg_color, scheme.foreground, "#bbbbbb"),
		success = first_non_nil(ansi[3], brights[3], scheme.foreground, "#00ff00"),
		active_badge_bg = first_non_nil(brights[5], ansi[5], active_tab.bg_color, scheme.background, "#777777"),
		active_badge_fg = first_non_nil(active_tab.fg_color, scheme.background, "#000000"),
		inactive_badge_bg = first_non_nil(ansi[1], inactive_tab.bg_color, scheme.background, "#333333"),
		inactive_badge_fg = first_non_nil(inactive_tab.fg_color, scheme.foreground, "#cccccc"),
	}
end

local palette = palette_from_scheme(color_scheme_name)
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
