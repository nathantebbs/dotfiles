local wezterm = require("wezterm")

local config = wezterm.config_builder()

-- Shell. WezTerm would read the login shell out of the password database,
-- which is right only on a machine where chsh has already run. Naming the
-- shell here means a fresh clone gets bash before setup.sh does anything.
-- "-l" makes it a login shell, so ~/.bash_profile runs.
local function bash_path()
	for _, path in ipairs({ "/opt/homebrew/bin/bash", "/usr/local/bin/bash", "/bin/bash" }) do
		local f = io.open(path, "r")
		if f then
			f:close()
			return path
		end
	end
	return "/bin/sh"
end

config.default_prog = { bash_path(), "-l" }

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
