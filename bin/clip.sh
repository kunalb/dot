#!/bin/zsh
if [[ $XDG_SESSION_TYPE = "wayland" ]]
then
	grim -g "$(slurp)" - | wl-copy
else
	maim -s | xclip -selection clipboard -t image/png;
fi

