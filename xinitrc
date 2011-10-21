#~/.dropbox-dist/dropboxd &

xsetroot -cursor_name left_ptr
xmodmap ~/.xmodmap

#export TERM="xterm-256color"

feh --bg-fill ~/Pictures/mars/m03_02192785.jpg
/usr/bin/xscreensaver -no-splash &

#exec /usr/bin/xmonad

DEFAULTWM='/usr/bin/xmonad'

case $1 in
xmonad)
    exec /usr/bin/xmonad
    ;;
twm)
    exec /usr/bin/twm
    ;;
*)
    exec $DEFAULTWM
    ;;
esac

