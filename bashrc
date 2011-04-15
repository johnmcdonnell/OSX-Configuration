# I have used sudo mdutil -a -i off to turn off spotlight.
# Use sudo mdutil -a -i on to turn it back on.


# shell variables
export PS1='\s-\v: \W \$ '
export PS1='\[\033k\033\\\]'$PS1

export JohnTools='/Library/Frameworks/Python.framework/Versions/6.0.4/lib/python2.6/johntools.py'

# Function file
if [ -f ~/bash_stuff/bash_functions.sh ]; then
. ~/bash_stuff/bash_functions.sh
fi


# Directory aliases
alias weather='~/Experiments/weather_trunk/'
alias expserv='cd /Volumes/Experiments/john/Weather2'
alias nsf='cd ~/Documents/NSFapp'

# command aliases
alias bibtexformat='perl /Users/j/Documents/Documentation/bibtexformat/bibtexformat -s'
alias cs2010='cd /Users/j/Experiments/weather_trunk/Gabor_cogsci2010'
alias analysis='cd /Users/j/Documents/Analysis'
alias qlf='qlmanage -p "$@" >& /dev/null'
alias t='~/bash_stuff/todo/todo.sh'
alias todos='python ~/bash_stuff/todo/birdseye.py ~/bash_stuff/todo.txt ~/bash_stuff/todo/done.txt'
alias twitter='perl ~/bash_stuff/twitter/twitter.pl'
alias growl='~/bash_stuff/galarm'
alias mplay='~/bash_stuff/mplay.sh'
alias mplayer_scrobbled='perl ~/bash_stuff/mplayer-lastfm-0.2.7.pl'
alias daft='mplayer_scrobbled -playlist /Users/j/Music/iTunes/iTunes\ Music/Daft\ Punk/Discovery/discovery.playlist'
alias xkcd='curl xkcd.com | grep hotlinking | awk {print\ \$5} | sed "s/<\/h3>//" > tmp.o && curl `cat tmp.o` > tmp.png || rm tmp.o && feh tmp.png || rm tmp.png'
alias connecthome='sudo airport -A thewired --password= lpid2yfmfVBFcg4KoDnA'
alias pygamepy='/opt/local/bin/python2.4'
alias chromeflix='open /Applications/Google\ Chrome.app --args -user-agent="Mozilla/5.0 (Macintosh; U; Intel Mac OS X 10_5_3; en-us) AppleWebKit/525.1 9 (KHTML, like Gecko) Version/3.2.1 Safari/525.19"'

alias fixnewlines="perl -pi -e 's/\r/\n/g'"

# Vim stuff
alias vless='vim -u /usr/share/vim/vim73/macros/less.vim'
#alias mvim='mvim --connect-silent'

# Ignore these common/boring commands in the history
export HISTIGNORE="&:ls:[bf]g"

# enable UTF-8 support by adding to your ~/.bash_login (or alternatives):
# /usr/bin/locale -a
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

# neat stuff:

alias ascii='less /usr/share/misc/ascii'

# Colors for ls
export CLICOLOR=1
export LSCOLORS=ExFxCxDxBxegedabagacad

#-------------------------------------------------------------
# tailoring 'less'
#-------------------------------------------------------------

alias more='less'
export PAGER=less
#export LESSCHARSET='latin1'   # I should maybe actually reconsider this
export LESSOPEN='|/usr/bin/lesspipe.sh %s 2>&-'
# Use this if lesspipe.sh exists
export LESS='-i -N -w  -z-4 -g -e -M -X -F -R -P%t?f%f :stdin .?pb%pb\%:?lbLine %lb:?bbByte %bb:-...'

# Use System frameworks (mostly for pyobjc)
#export PYTHONPATH='/System/Library/Frameworks/Python.framework/Versions/2.6/Extras/lib/python'

# Handy stuff for ipython
export LESS="-R" # Tells less to use raw input
export EDITOR=vim
export BROWSER=midori

# PATH for port.
# REQUIRES GCC!!!
export PATH=/opt/local/bin:$PATH
export MANPATH=/opt/local/share/man:$MANPATH
export INFOPATH=/opt/local/share/info:$INFOPATH

# Setting some options
shopt -s globstar autocd
shopt -s expand_aliases 
shopt -s cmdhist

# Scripts folder
export PATH=/opt/scripts:$PATH

# Godi for OCaml
export PATH="/opt/godi/bin:/opt/godi/sbin:$PATH";
export MANPATH="/opt/godi/man:$MANPATH";

# Cabal for Haskell
export PATH="Users/j/.cabal/bin:$PATH";

# Mathematica
export PATH="/Applications/Mathematica.app/Contents/MacOS/:$PATH";

# Android debugger
export PATH=/Users/j/source/android-sdk-mac/tools:$PATH

# Setting PATH for /opt/python
export PATH="/opt/python/bin:$PATH"
export PKG_CONFIG_PATH="/opt/python/lib/pkgconfig"
export PATH="/Library/Frameworks/Python.framework/Versions/6.0.4/bin/:$PATH"

# Make X11 defaults work.
#xrdb -merge ~/.Xdefaults &

if [ -f /opt/local/etc/bash_completion ]; then
    . /opt/local/etc/bash_completion
fi

