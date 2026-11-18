-- Migrated from tmux, Herdr, Ghostty, kitty and WezTerm.

-- tmux (~/.config/tmux/tmux.conf), prefix ctrl+z
rex.bind("ctrl+z>ctrl+z", "pane.send_key", { key = "ctrl+z" })
rex.bind("ctrl+z>s", "pane.split", { direction = "right" })
rex.bind("ctrl+z>shift+s", "pane.split", { direction = "down" })
rex.bind("ctrl+z>w", "pane.close")
rex.bind("ctrl+z>h", "pane.focus", { direction = "left" })
rex.bind("ctrl+z>j", "pane.focus", { direction = "down" })
rex.bind("ctrl+z>k", "pane.focus", { direction = "up" })
rex.bind("ctrl+z>l", "pane.focus", { direction = "right" })
rex.bind("ctrl+z>ctrl+p", "client.tab.previous")
rex.bind("ctrl+z>ctrl+n", "client.tab.next")
rex.bind("ctrl+z>r", "client.config.reload")
rex.bind("ctrl+z>x", "pane.close")

-- Herdr (~/.config/herdr/config.toml), prefix ctrl+a
rex.bind("ctrl+a>ctrl+a", "pane.send_key", { key = "ctrl+a" })
rex.bind("ctrl+a>p", "session.previous")
rex.bind("ctrl+a>n", "session.next")

-- WezTerm (~/.config/wezterm/wezterm.lua), leader ctrl+z
rex.bind("ctrl+z>c", "client.tab.new")
for i = 1, 9 do
  rex.bind("ctrl+z>" .. i, "client.tab.goto", { index = i })
end
