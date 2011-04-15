#!/bin/sh

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/usr/X11/lib/X11/xinit/.Xresources
sysmodmap=/usr/X11/lib/X11/xinit/.Xmodmap

# merge in defaults and keymaps

if [ -f $sysresources ]; then

    if [ -x /usr/bin/cpp ] ; then
        xrdb -merge $sysresources
    else
        xrdb -nocpp -merge $sysresources
    fi



fi

if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi

if [ -f "$userresources" ]; then

    if [ -x /usr/bin/cpp ] ; then
        xrdb -merge "$userresources"
    else
        xrdb -nocpp -merge "$userresources"
    fi



fi

if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi

# start some nice programs




#if [ -d /usr/X11/lib/X11/xinit/xinitrc.d ] ; then
#	for f in /usr/X11/lib/X11/xinit/xinitrc.d/*.sh ; do
#		[ -x "$f" ] && . "$f"
#	done
#	unset f
#fi


xrdb -merge $HOME/.Xdefaults
#twm &#
#xclock -geometry 50x50-1+1 &
#xterm -geometry 80x50+494+51 &
#xterm -geometry 80x20+494-0 &
/usr/X11/lib/X11/xinit/xinitrc.d/10-fontdir.sh
xset fp+ ~/.fonts
xset fp rehash
cd
quartz-wm --only-proxy &
source ~/.zshenv
echo PATH=/Users/j/.cabal/bin/:$PATH
export BROWSER=midori

# Desktop Background
feh --bg-scale ~/Pictures/Desktops/Two\ Robots\ named\ Daft\ Punk.jpg &

# Some settings for dzen
FG='#a8a3f7' 
BG='#3f3c6d'
off=0
touch /tmp/mail
export MOZ_PLUGIN_PATH="/Users/j/.config/midori/plugin"
/Users/j/bash_stuff/dzen/status.sh | dzen2 -l 5 -x 400 -w $off -ta r -fg $FG -bg $BG -p -e 'sigusr1=raise;sigusr2=lower;onquit=exec:rm /tmp/dzen2-pid' & echo $! > /tmp/dzen2-pid
#python /Users/j/bash_stuff/dzen/status.py | dzen2 -l 5 -x 400 -w $off -ta r -fg $FG -bg $BG -p -e 'sigusr1=raise;sigusr2=lower;onquit=exec:rm /tmp/dzen2-pid' & echo $! > /tmp/dzen2-pid

#/Users/j/bash_stuff/dzen/status.sh | dzen2 -ta r -fn '-*-profont-*-*-*-*-13-*-*-*-*-*-iso8859' -bg '#2c2c32' -fg 'grey70' -p -e '' &
#/Users/j/bash_stuff/dzen/status.sh | dzen2 -ta r -bg '#2c2c32' -fg 'grey70' -p -e '' &
/Users/j/.cabal/bin/xmonad
#exec /opt/local/bin/ratpoison
#/opt/local/bin/wmaker &
#/opt/local/bin/wmii welcome &
#exec xterm

# exec xterm -geometry 80x66+0+0 -name login
