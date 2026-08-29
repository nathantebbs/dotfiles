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

-- Cursor. Vim and Emacs both reshape the cursor per mode with DECSCUSR; this
-- is only what an application that says nothing gets.
config.default_cursor_style = "SteadyBlock"

-- Transparency
config.window_background_opacity = 0.92

-- Chrome
config.hide_tab_bar_if_only_one_tab = true
config.window_close_confirmation = "NeverPrompt"

-- Scrollback. Panes here do the job tmux used to, so it inherits the
-- history-limit tmux.conf set.
config.scrollback_lines = 10000

-- Vim rings the bell for a failed search and for hitting the end of a buffer.
config.audible_bell = "Disabled"

-- KEYBOARD:

-- Advertise the kitty keyboard protocol. Terminals cannot otherwise tell <C-i>
-- from <Tab> or <C-[> from <Esc>, because they arrive as the same byte. Opt-in
-- per application, so anything that does not ask is unaffected.
config.enable_kitty_keyboard = true

-- KEYS:

-- Everything below hangs off CMD. It is the one modifier macOS never forwards
-- to the terminal, so no binding here can shadow a Vim, Emacs or readline key.
-- The cost is CMD+h, which stops hiding the app; CMD+m still does that.
if wezterm.target_triple:find("apple") then
	-- Keep Left Option available as Meta. Right Option still composes accents.
	config.send_composed_key_when_left_alt_is_pressed = false
	config.send_composed_key_when_right_alt_is_pressed = true
	config.use_dead_keys = false

	local act = wezterm.action
	config.keys = {
	-- Splits. The pane inherits the cwd, which is what tmux's -c did.
	{ key = "d", mods = "CMD", action = act.SplitHorizontal({ domain = "CurrentPaneDomain" }) },
	{ key = "d", mods = "CMD|SHIFT", action = act.SplitVertical({ domain = "CurrentPaneDomain" }) },

	-- Focus a pane, hjkl.
	{ key = "h", mods = "CMD", action = act.ActivatePaneDirection("Left") },
	{ key = "j", mods = "CMD", action = act.ActivatePaneDirection("Down") },
	{ key = "k", mods = "CMD", action = act.ActivatePaneDirection("Up") },
	{ key = "l", mods = "CMD", action = act.ActivatePaneDirection("Right") },

	-- Resize a pane. CMD|CTRL rather than CMD|SHIFT, which clear-scrollback
	-- wants below. Aerospace holds CMD|CTRL|ALT, so it does not collide.
	{ key = "h", mods = "CMD|CTRL", action = act.AdjustPaneSize({ "Left", 3 }) },
	{ key = "j", mods = "CMD|CTRL", action = act.AdjustPaneSize({ "Down", 3 }) },
	{ key = "k", mods = "CMD|CTRL", action = act.AdjustPaneSize({ "Up", 3 }) },
	{ key = "l", mods = "CMD|CTRL", action = act.AdjustPaneSize({ "Right", 3 }) },

	{ key = "z", mods = "CMD", action = act.TogglePaneZoomState },
	{ key = "x", mods = "CMD", action = act.CloseCurrentPane({ confirm = true }) },

	-- Tabs. CMD+t, CMD+w and CMD+1..9 are already right by default; these are
	-- the unshifted aliases for the relative motions.
	{ key = "[", mods = "CMD", action = act.ActivateTabRelative(-1) },
	{ key = "]", mods = "CMD", action = act.ActivateTabRelative(1) },

	-- Displaced by pane focus above.
	{ key = "k", mods = "CMD|SHIFT", action = act.ClearScrollback("ScrollbackOnly") },

	-- Copy mode navigates with hjkl, v and y, so it needs no configuring.
	{ key = "x", mods = "CMD|SHIFT", action = act.ActivateCopyMode },

	-- Label every path, URL and hash on screen and jump to one by typing its
	-- letters. The keyboard answer to reaching for the mouse.
	{ key = "Space", mods = "CMD|SHIFT", action = act.QuickSelect },

	{ key = "Enter", mods = "CMD", action = act.ToggleFullScreen },
	}
end


-- Stop WezTerm from resizing the window frame on font zoom
config.adjust_window_size_when_changing_font_size = false

return config
