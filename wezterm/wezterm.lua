local wezterm = require 'wezterm'
local config = {}

-- Cosmic Latte - Warm Light Theme
local cosmic_latte = {
    foreground = '#000000',
    background = '#FFF8E7',
    cursor_fg = '#FFF8E7',
    cursor_bg = '#000000',
    cursor_border = '#000000',
    selection_fg = '#FFF8E7',
    selection_bg = '#1a1a1a',
    scrollbar_thumb = '#4a4a4a',
    split = '#666666',
    ansi = {
        '#000000',  -- black
        '#a80000',  -- red
        '#006800',  -- green
        '#a65000',  -- yellow/amber
        '#0030a0',  -- blue
        '#800080',  -- magenta
        '#006878',  -- cyan
        '#505050',  -- white
    },
    brights = {
        '#303030',  -- bright black
        '#d00000',  -- bright red
        '#008000',  -- bright green
        '#b86200',  -- bright yellow/amber
        '#0040d0',  -- bright blue
        '#a000a0',  -- bright magenta
        '#008090',  -- bright cyan
        '#000000',  -- bright white
    },
}

-- Cosmic Espresso - Warm Dark Theme
local cosmic_espresso = {
    foreground = '#FFF8E7',
    background = '#1a1614',
    cursor_fg = '#1a1614',
    cursor_bg = '#FFF8E7',
    cursor_border = '#FFF8E7',
    selection_fg = '#1a1614',
    selection_bg = '#FFF8E7',
    scrollbar_thumb = '#4a4a4a',
    split = '#3a3634',
    ansi = {
        '#2a2624',  -- black
        '#ff6b6b',  -- red
        '#7ec87e',  -- green
        '#e8a64a',  -- yellow/amber
        '#6b9bff',  -- blue
        '#d07bd0',  -- magenta
        '#5bbfc9',  -- cyan
        '#a09a90',  -- white
    },
    brights = {
        '#4a4644',  -- bright black
        '#ff8a8a',  -- bright red
        '#98e098',  -- bright green
        '#f0b860',  -- bright yellow/amber
        '#8ab4ff',  -- bright blue
        '#e094e0',  -- bright magenta
        '#78d0d8',  -- bright cyan
        '#FFF8E7',  -- bright white
    },
}

-- Toggle: 'light' or 'dark'
local theme = 'dark'
config.colors = theme == 'light' and cosmic_latte or cosmic_espresso

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

config.hyperlink_rules = wezterm.default_hyperlink_rules()
table.insert(
   config.hyperlink_rules,
   {regex = [[\b(D\d+)\b]], format = 'https://www.internalfb.com/diff/$1'}
)

return config
