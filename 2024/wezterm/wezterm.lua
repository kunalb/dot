local wezterm = require 'wezterm'
local config = {}

-- config.color_scheme = 'Ura (Gogh)' -- 'Clrs (Gogh)'
config.color_scheme = 'Aura (Gogh)' -- 'Argonaut (Gogh)' -- Argonaut
config.font = wezterm.font 'Berkeley Mono'
config.font_size = 10
config.line_height = 1.0
config.audible_bell = "Disabled"
config.enable_tab_bar = false
config.freetype_load_target = "Light"
config.window_padding = {
   left = 0,
   right = 0,
   top = 0,
   bottom = 0
}
-- config.dpi = 96.0

config.hyperlink_rules = wezterm.default_hyperlink_rules()
table.insert(
   config.hyperlink_rules,
   {regex = [[\b(D\d+)\b]], format = 'https://www.internalfb.com/diff/$1'}
)

return config
