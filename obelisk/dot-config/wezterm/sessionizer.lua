local wezterm = require("wezterm")
local mux = wezterm.mux
local act = wezterm.action

local M = {}

local function normalize_workspace_name(project_path)
	local session_name = project_path:match("([^/]+)$") or project_path
	session_name = session_name:gsub("%s+", "")
	session_name = session_name:gsub("%.", "-", 1)
	session_name = session_name:gsub("%(.*$", "")
	session_name = session_name:gsub("%{.*$", "")
	session_name = session_name:gsub("%[.*$", "")
	return session_name
end

local function list_projects_from_zoxide()
	local success, stdout, stderr = wezterm.run_child_process({
		"/bin/zsh",
		"-lc",
		"zoxide query --list --score",
	})

	if not success then
		return nil, "Failed to run zoxide: " .. stderr
	end

	local projects = {}

	for line in stdout:gmatch("[^\r\n]+") do
		local score, path = line:match("^%s*([%d%.]+)%s+(.+)%s*$")
		if path then
			local normalized_path = path:gsub("^[^/]*", "")
			table.insert(projects, { path = normalized_path, score = tonumber(score) or 0 })
		elseif line:match("^%s*/") then
			table.insert(projects, { path = line, score = 0 })
		end
	end

	table.sort(projects, function(a, b)
		if a.score == b.score then
			return a.path < b.path
		end
		return a.score > b.score
	end)

	return projects
end

local function workspace_exists(workspace_name)
	for _, existing_name in ipairs(mux.get_workspace_names()) do
		if existing_name == workspace_name then
			return true
		end
	end
	return false
end

local function notify(window, title, message)
	if window and window.toast_notification then
		local ok = pcall(function()
			window:toast_notification(title, message, nil, 4000)
		end)
		if ok then
			return
		end
	end
	wezterm.log_info(title .. ": " .. message)
end

M.toggle = function(window, pane)
	local projects, err = list_projects_from_zoxide()

	if not projects then
		wezterm.log_error(err)
		notify(window, "sessionizer", "Failed to read project list from zoxide")
		return
	end

	if #projects == 0 then
		notify(window, "sessionizer", "No zoxide entries found")
		return
	end

	local choices = {}
	for _, project in ipairs(projects) do
		table.insert(choices, {
			id = project.path,
			label = project.path,
		})
	end

	window:perform_action(
		act.InputSelector({
			action = wezterm.action_callback(function(win, callback_pane, id, label)
				local selected_project = label or id

				if not id and not label then
					wezterm.log_info("No project selected. Exiting.")
				else
					selected_project = selected_project:gsub("^[^/]*", "")
					local session_name = normalize_workspace_name(selected_project)

					wezterm.log_info("Selected " .. selected_project)

					if workspace_exists(session_name) then
						win:perform_action(act.SwitchToWorkspace({ name = session_name }), callback_pane or pane)
					else
						win:perform_action(
							act.SwitchToWorkspace({ name = session_name, spawn = { cwd = selected_project } }),
							callback_pane or pane
						)
					end
				end
			end),
			fuzzy = true,
			title = "Select Path",
			choices = choices,
		}),
		pane
	)
end

M.switch_session = function(window, pane)
	local workspaces = mux.get_workspace_names()

	if #workspaces == 0 then
		notify(window, "sessionizer", "No open sessions found")
		return
	end

	table.sort(workspaces, function(a, b)
		return a < b
	end)

	local choices = {}
	for _, workspace in ipairs(workspaces) do
		table.insert(choices, {
			id = workspace,
			label = workspace,
		})
	end

	window:perform_action(
		act.InputSelector({
			action = wezterm.action_callback(function(win, callback_pane, id, label)
				local workspace = label or id

				if not workspace then
					wezterm.log_info("No session selected. Exiting.")
					return
				end

				win:perform_action(act.SwitchToWorkspace({ name = workspace }), callback_pane or pane)
			end),
			fuzzy = true,
			title = "Select Session",
			choices = choices,
		}),
		pane
	)
end

return M
