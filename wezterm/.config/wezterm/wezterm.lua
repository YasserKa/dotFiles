local wezterm = require("wezterm")
local mux = wezterm.mux
local config = {
	adjust_window_size_when_changing_font_size = false,
	window_background_opacity = 0.9,
	hide_tab_bar_if_only_one_tab = true,
	color_scheme = "Gruvbox dark, hard (base16)",
	font = wezterm.font("Inconsolata Nerd Font"),
	font_size = 14,
	warn_about_missing_glyphs = false,

	window_padding = {
		left = 5,
		right = 5,
		top = 5,
		bottom = 5,
	},
	keys = {
		{
			key = "=",
			mods = "CTRL",
			action = wezterm.action.ResetFontSize,
		},
	},
}
return config
