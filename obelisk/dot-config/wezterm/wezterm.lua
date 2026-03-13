local wezterm = require("wezterm")
local sessionizer = require("sessionizer")
local config = {}
local act = wezterm.action

-- ============================================================================
-- GLOBAL STATE INITIALIZATION
-- ============================================================================
-- We use wezterm.GLOBAL to store variables that persist across all windows.
-- This allows us to track UI toggles and history without losing state.
if wezterm.GLOBAL.show_workspaces == nil then
	wezterm.GLOBAL.show_workspaces = false
end

-- ============================================================================
-- THEME AND COLOR CONFIGURATION
-- ============================================================================
local color_scheme_name = "oxocarbon"
config.color_scheme = color_scheme_name
local custom_schemes = {}

-- Safely attempt to load the local custom oxocarbon theme file.
-- If it fails (e.g., file missing), it won't crash the terminal.
local ok, loaded_oxocarbon = pcall(function()
	return wezterm.color.load_scheme(wezterm.config_dir .. "/colors/oxocarbon.toml")
end)
if ok and loaded_oxocarbon then
	custom_schemes.oxocarbon = loaded_oxocarbon
	config.color_schemes = custom_schemes
end

-- Helper function to return the first non-nil value (like a fallback chain)
local function first_non_nil(...)
	for i = 1, select("#", ...) do
		local value = select(i, ...)
		if value ~= nil then
			return value
		end
	end
	return nil
end

-- Extracts specific colors from the loaded theme to build our custom UI palette.
-- This ensures our custom workspace and tab pills match the active theme perfectly.
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

-- UI styling variables for our pill shapes and icons
local rounded_left = ""
local rounded_right = ""
local pill_gap = " "
local workspace_icon = " "

-- ============================================================================
-- TAB & WORKSPACE HISTORY TRACKING
-- ============================================================================
local tab_history_by_window = {}

-- Finds the index of the currently active tab within a specific window
local function get_active_tab_index(window)
	local mux_window = window:mux_window()
	if not mux_window then
		return nil
	end
	for idx, tab_info in ipairs(mux_window:tabs_with_info()) do
		if tab_info.is_active then
			return idx - 1
		end
	end
	return nil
end

-- Records the current and previous tab to allow jumping back and forth
local function track_tab_history(window)
	local window_id = window:window_id()
	local active_tab_index = get_active_tab_index(window)
	if active_tab_index == nil then
		return
	end

	local history = tab_history_by_window[window_id]
	if not history then
		tab_history_by_window[window_id] = { current = active_tab_index, previous = nil }
		return
	end

	if history.current ~= active_tab_index then
		history.previous = history.current
		history.current = active_tab_index
	end
end

-- Action function to switch to the previously active tab
local function switch_to_previous_tab(window, pane)
	local history = tab_history_by_window[window:window_id()]
	if not history or history.previous == nil then
		return
	end

	local target = history.previous
	history.previous = history.current
	history.current = target
	window:perform_action(act.ActivateTab(target), pane)
end

-- Determines what text to display on a tab (user title or active program)
local function tab_title(tab_info)
	local title = tab_info.tab_title
	if title and #title > 0 then
		return title
	end
	return tab_info.active_pane.title
end

-- ============================================================================
-- CUSTOM EVENTS
-- ============================================================================
wezterm.on("sessionizer", function(window, pane)
	sessionizer.toggle(window, pane)
end)

wezterm.on("sessionizer-switch", function(window, pane)
	sessionizer.switch_session(window, pane)
end)

wezterm.on("previous-tab", function(window, pane)
	switch_to_previous_tab(window, pane)
end)

-- ============================================================================
-- DYNAMIC UI: WORKSPACES VS TABS STATUS BAR
-- ============================================================================
-- This event fires continuously in the background to update the UI
wezterm.on("update-status", function(window, pane)
	-- 1. Track Tab History
	track_tab_history(window)

	-- 2. Track Workspace History (Used for Option+Tab functionality)
	local current_workspace = window:active_workspace()
	if not wezterm.GLOBAL.current_workspace then
		wezterm.GLOBAL.current_workspace = current_workspace
	elseif current_workspace ~= wezterm.GLOBAL.current_workspace then
		wezterm.GLOBAL.previous_workspace = wezterm.GLOBAL.current_workspace
		wezterm.GLOBAL.current_workspace = current_workspace
	end

	-- 3. Render the UI based on the Option+§ toggle state
	local status_elements = {}

	if wezterm.GLOBAL.show_workspaces then
		-- >> WORKSPACE VIEW ACTIVE <<
		-- Fetch all workspaces, sort them alphabetically, and display them as pills.
		local workspaces = wezterm.mux.get_workspace_names()
		table.sort(workspaces)

		for index, name in ipairs(workspaces) do
			local is_active = (name == current_workspace)
			-- Highlight the active workspace in 'success' color, mute the others
			local bg_color = is_active and palette.success or palette.surface_alt
			local fg_color = is_active and palette.bg or palette.text

			table.insert(status_elements, { Background = { Color = palette.bg } })
			table.insert(status_elements, { Foreground = { Color = bg_color } })
			table.insert(status_elements, { Text = rounded_left })

			table.insert(status_elements, { Background = { Color = bg_color } })
			table.insert(status_elements, { Foreground = { Color = fg_color } })
			table.insert(status_elements, { Attribute = { Intensity = is_active and "Bold" or "Normal" } })

			-- Include the number shortcut (1-9) so the user knows what Option+Number to press
			local prefix = is_active and (workspace_icon .. index) or tostring(index)
			table.insert(status_elements, { Text = " " .. prefix .. ": " .. name .. " " })

			table.insert(status_elements, { Background = { Color = palette.bg } })
			table.insert(status_elements, { Foreground = { Color = bg_color } })
			table.insert(status_elements, { Text = rounded_right })

			table.insert(status_elements, { Background = { Color = palette.bg } })
			table.insert(status_elements, { Text = pill_gap })
		end
	else
		-- >> DEFAULT TAB VIEW ACTIVE <<
		-- Only show the current active workspace pill on the far left. The standard tabs follow it.
		table.insert(status_elements, { Background = { Color = palette.bg } })
		table.insert(status_elements, { Text = pill_gap })
		table.insert(status_elements, { Foreground = { Color = palette.success } })
		table.insert(status_elements, { Background = { Color = palette.bg } })
		table.insert(status_elements, { Text = rounded_left })
		table.insert(status_elements, { Background = { Color = palette.success } })
		table.insert(status_elements, { Foreground = { Color = palette.bg } })
		table.insert(status_elements, { Text = " " .. workspace_icon .. " " })
		table.insert(status_elements, { Background = { Color = palette.surface_alt } })
		table.insert(status_elements, { Foreground = { Color = palette.text } })
		table.insert(status_elements, { Text = " " .. current_workspace .. " " })
		table.insert(status_elements, { Foreground = { Color = palette.surface_alt } })
		table.insert(status_elements, { Background = { Color = palette.bg } })
		table.insert(status_elements, { Text = rounded_right })
		table.insert(status_elements, { Background = { Color = palette.bg } })
		table.insert(status_elements, { Text = pill_gap })
	end

	-- Apply the constructed elements to the left side of the tab bar
	window:set_left_status(wezterm.format(status_elements))
	window:set_right_status("")
end)

-- Custom styling for the actual tabs
wezterm.on("format-tab-title", function(tab, _tabs, _panes, _cfg, _hover, max_width)
	-- Instant visual override: If Workspace view is active, camouflage the tabs entirely
	-- so they blend into the background and "disappear" without causing lag.
	if wezterm.GLOBAL.show_workspaces then
		return {
			{ Background = { Color = palette.bg } },
			{ Foreground = { Color = palette.bg } },
			{ Text = " " },
		}
	end

	-- Format the title and truncate if it gets too long
	local title = tab_title(tab)
	local title_width = math.max(8, max_width - 7)
	if #title > title_width then
		title = wezterm.truncate_right(title, title_width)
	end

	-- Determine colors based on whether this tab is currently active
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

	-- Build and return the styled tab pill
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

-- ============================================================================
-- FONTS AND WINDOW CONFIGURATION
-- ============================================================================
config.font = wezterm.font("Iosevka")
config.font_size = 13

config.hide_tab_bar_if_only_one_tab = false
config.use_fancy_tab_bar = false
config.show_new_tab_button_in_tab_bar = false
config.tab_max_width = 56
config.window_padding = { left = 3, right = 0, top = 3, bottom = 0 }

-- Apply our palette to the window's basic tab bar elements
config.colors = {
	compose_cursor = palette.active_badge_bg,
	tab_bar = {
		background = palette.bg,
		active_tab = { bg_color = palette.surface_alt, fg_color = palette.text },
		inactive_tab = { bg_color = palette.surface, fg_color = palette.muted },
		inactive_tab_hover = { bg_color = palette.surface_alt, fg_color = palette.text },
		new_tab = { bg_color = palette.bg, fg_color = palette.muted },
		new_tab_hover = { bg_color = palette.surface_alt, fg_color = palette.text },
	},
}

config.default_prog = { "zsh" }
config.enable_wayland = false
config.window_decorations = "RESIZE"

-- IMPORTANT: Left alt must not send composed keys (like special macOS characters).
-- This ensures our OPT bindings fire correctly instead of typing symbols.
config.send_composed_key_when_left_alt_is_pressed = false
config.send_composed_key_when_right_alt_is_pressed = true

config.max_fps = 240

-- ============================================================================
-- KEYBINDINGS
-- ============================================================================
config.leader = { key = "z", mods = "CTRL", timeout_milliseconds = 2000 }
config.keys = {
	-- Leader shortcuts for sessionizer, tabs, and pane management
	{ key = "Space", mods = "LEADER", action = act.EmitEvent("sessionizer") },
	{ key = "z", mods = "LEADER|CTRL", action = act.EmitEvent("sessionizer-switch") },
	{ key = "c", mods = "LEADER", action = act.SpawnTab("CurrentPaneDomain") },
	{ key = "s", mods = "LEADER", action = act.SplitHorizontal({ domain = "CurrentPaneDomain" }) },
	{ key = "S", mods = "LEADER|SHIFT", action = act.SplitVertical({ domain = "CurrentPaneDomain" }) },
	{ key = "Tab", mods = "LEADER", action = act.EmitEvent("previous-tab") },
	{ key = "h", mods = "LEADER", action = act.ActivatePaneDirection("Left") },
	{ key = "j", mods = "LEADER", action = act.ActivatePaneDirection("Down") },
	{ key = "k", mods = "LEADER", action = act.ActivatePaneDirection("Up") },
	{ key = "l", mods = "LEADER", action = act.ActivatePaneDirection("Right") },

	-- OPTION + §: Toggle Workspace/Tab Bar View (Instant/Zero-Lag)
	-- Flips the global boolean and forces a fast UI refresh.
	{
		key = "§",
		mods = "OPT",
		action = wezterm.action_callback(function(window, pane)
			wezterm.GLOBAL.show_workspaces = not wezterm.GLOBAL.show_workspaces
			window:set_left_status("")
		end),
	},

	-- OPTION + 0: Close Current Workspace Safely
	{
		key = "0",
		mods = "OPT",
		action = wezterm.action_callback(function(window, pane)
			local current_workspace = window:active_workspace()
			local next_workspace = wezterm.GLOBAL.previous_workspace

			-- Fallback: If there is no previous, find the first available one that isn't current
			if not next_workspace or next_workspace == current_workspace then
				local workspaces = wezterm.mux.get_workspace_names()
				for _, w in ipairs(workspaces) do
					if w ~= current_workspace then
						next_workspace = w
						break
					end
				end
			end

			-- Safety net: If absolutely no other workspace exists, fall back to "default"
			if not next_workspace or next_workspace == current_workspace then
				next_workspace = "default"
			end

			-- Switch out before killing it to avoid closing the entire WezTerm GUI application
			window:perform_action(act.SwitchToWorkspace({ name = next_workspace }), pane)

			-- Hunt down and safely kill all background panes attached to the closed workspace
			for _, mux_window in ipairs(wezterm.mux.all_windows()) do
				if mux_window:get_workspace() == current_workspace then
					for _, tab in ipairs(mux_window:tabs()) do
						for _, p in ipairs(tab:panes()) do
							wezterm.run_child_process({
								"wezterm",
								"cli",
								"kill-pane",
								"--pane-id",
								tostring(p:pane_id()),
							})
						end
					end
				end
			end
		end),
	},

	-- OPTION + TAB: Jump to previous workspace seamlessly
	{
		key = "Tab",
		mods = "OPT",
		action = wezterm.action_callback(function(window, pane)
			local prev_workspace = wezterm.GLOBAL.previous_workspace
			if prev_workspace and prev_workspace ~= window:active_workspace() then
				window:perform_action(act.SwitchToWorkspace({ name = prev_workspace }), pane)
			end
		end),
	},
}

-- LEADER + 1-9: Keep original bindings for jumping directly to TABS
for i = 1, 9 do
	table.insert(config.keys, {
		key = tostring(i),
		mods = "LEADER",
		action = act.ActivateTab(i - 1),
	})
end

-- OPTION + 1-9: Context-Aware Navigation
-- If the UI is showing workspaces, these keys jump to workspaces.
-- If the UI is showing tabs, these keys jump to tabs.
for i = 1, 9 do
	table.insert(config.keys, {
		key = tostring(i),
		mods = "OPT",
		action = wezterm.action_callback(function(window, pane)
			if wezterm.GLOBAL.show_workspaces then
				-- Workspaces view is toggled ON: Switch workspaces
				local workspaces = wezterm.mux.get_workspace_names()
				table.sort(workspaces)
				local target_workspace = workspaces[i]
				if target_workspace then
					window:perform_action(act.SwitchToWorkspace({ name = target_workspace }), pane)
				end
			else
				-- Default view is ON: Switch tabs
				window:perform_action(act.ActivateTab(i - 1), pane)
			end
		end),
	})
end

return config
