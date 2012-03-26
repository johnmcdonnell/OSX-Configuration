
#~/.dropbox-dist/dropboxd &

export PRINTER='raw@128.122.115.239'

xsetroot -cursor_name left_ptr
xmodmap ~/.xmodmap

#export TERM="xterm-256color"

# Try to get gnome-keyring-daemon to work
eval "`gnome-keyring-daemon`"
export GNOME_KEYRING_PID
export GNOME_KEYRING_SOCKET


gsynaptics-init &

# Choose a background
bash -c 'files=(/home/mcdon/Pictures/mars/*); feh --bg-fill ${files[RANDOM % ${#files[@]}]}'
#feh --bg-fill ~/Pictures/mars/m03_02192785.jpg

#/usr/bin/xscreensaver -no-splash &
#/home/mcdon/.xscreenwatch &
xbindkeys & 

xset dpms 150 0 300  # Screen sleep
xinput --set-prop 'Apple Magic Mouse' 'Device Accel Constant Deceleration' 3 2

#exec /usr/bin/xmonad

#DEFAULTWM='/usr/bin/xmonad'

case $1 in
xmonad)
    exec ck-launch-session dbus-launch /usr/bin/xmonad
    ;;
openbox)
    exec /usr/bin/openbox-session
    ;;
twm)
    exec /usr/bin/twm
    ;;
*)
    exec ck-launch-session dbus-launch /usr/bin/xmonad
    ;;
esac

kill $GNOME_KEYRING_PID
