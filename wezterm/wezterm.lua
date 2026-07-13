local wezterm = require("wezterm")

local config = wezterm.config_builder()

-- Font
config.font = wezterm.font("Zenbones Brainy")
config.font_size = 16.0

-- Cursor
config.default_cursor_style = "SteadyBlock"

-- Transparency
config.window_background_opacity = 0.92

-- Chrome
config.hide_tab_bar_if_only_one_tab = true
config.window_close_confirmation = "NeverPrompt"

return config
