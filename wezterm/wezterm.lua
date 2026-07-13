local wezterm = require("wezterm")

local config = wezterm.config_builder()

-- Font. Zenbones Brainy has no icon glyphs, so fall back to Symbols Nerd Font
-- Mono (fonts/NFM.ttf, installed by util/scripts/install-fonts.sh) for the
-- powerline separators and devicons that prompts and statuslines use.
config.font = wezterm.font_with_fallback({
	"Zenbones Brainy",
	"Symbols Nerd Font Mono",
})
config.font_size = 14.0

-- Cursor
config.default_cursor_style = "SteadyBlock"

-- Transparency
config.window_background_opacity = 0.92

-- Chrome
config.hide_tab_bar_if_only_one_tab = true
config.window_close_confirmation = "NeverPrompt"

return config
