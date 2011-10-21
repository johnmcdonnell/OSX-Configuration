#~/.dropbox-dist/dropboxd &

#xmodmap -e "clear lock"
#xmodmap -e "clear Mod1"
#xmodmap -e "clear Mod2"
#xmodmap -e "add Mod1 = Meta_L Meta_R"
#xmodmap -e "add Mod2 = Mode_switch"
#xmodmap -e "pointer = 1 3 2 4 5 6 7" 
xsetroot -cursor_name left_ptr
xmodmap ~/.xmodmap

#export TERM="xterm-256color"

feh --bg-fill ~/Pictures/mars/m03_02192785.jpg

exec /usr/bin/xmonad

DEFAULTWM='/usr/bin/xmonad'

case $1 of
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

