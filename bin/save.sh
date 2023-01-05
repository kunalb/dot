#!/bin/sh

DEFAULT_NAME=$(date +%Y%m%d-%H%M%S.png)
OUTFILE="${1:-$DEFAULT_NAME}"


if [[ $XDG_SESSION_TYPE = "wayland" ]]
then
        wl-paste > $OUTFILE
else
        xclip -selection clipboard -t image/png -o > $OUTFILE
fi
