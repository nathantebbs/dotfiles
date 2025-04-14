local wezterm = require 'wezterm'

local config = wezterm.config_builder()

config.color_scheme = 'Gnometerm'

config.font = wezterm.font 'JetBrains Mono'

return config