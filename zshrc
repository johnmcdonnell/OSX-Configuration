
# term
if [ "x" != "x$DISPLAY" ]
then
   if [ "screen" = "$TERM" ]
   then
       export TERM=screen-256color
   else
       export TERM=xterm-256color
   fi
fi

#if [[ -z $STY ]]; then  # this might be dangerous.
#	exec screen -R
#fi

# shell variables
export PROMPT="[%~]
#%h %%"
export RPROMPT="[%h]"

# set prompt to cue command mode
# (http://zshwiki.org/home/examples/zlewidgets) 
function zle-line-init zle-keymap-select {
    RPS1="${${KEYMAP/vicmd/-- COMMAND --}/(main|viins)/}"
    RPS2=$RPS1
    zle reset-prompt
}
zle -N zle-line-init
zle -N zle-keymap-select

#source ~/.zprompt # Fancy prompt; don't really need it.

# set vim mode
bindkey -v
# rebind deletes
bindkey -M viins '^H' backward-delete-char
bindkey -M viins '^?' backward-delete-char
bindkey -a 'q' push-line # Escape out of a line for one command.

# Set up directory history
export DIRSTACKSIZE=10
setopt autopushd pushdminus pushdsilent pushdtohome
alias dv='dirs -v'
dirs /Users/j/Dropbox/Classes/DawClass/finalPaper/ /Users/j/Experiments/weather_trunk/TV_exp/Draw_fork /Users/j/Experiments/weather_trunk/TV_exp/Ordering_fork /Users/j/Experiments/weather_trunk/TV_exp/Unsupervised_fork /Users/j/Experiments/weather_trunk/SecondYearPaper

# Log universal history (potentially a security risk)
#export HISTFILE=/tmp/zhistory
#export SAVEHIST=150


# The following lines were added by compinstall

zstyle ':completion:*' completer _complete _ignored _correct _approximate
zstyle ':completion:*' format 'Completing %d'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' matcher-list '' 'm:{[:lower:]}={[:upper:]}' 'r:|[._-]=** r:|=**' 'l:|=* r:|=*'
zstyle ':completion:*' menu select=3
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle :compinstall filename '/Users/j/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

# Set some options:
setopt extendedglob
setopt numericglobsort
setopt noclobber # requires '>!' to overwrite files; automatically set  in history. 
# setopt ignoreeof # ignore EOF, requires 'exit' or 'logout' to exit.
setopt autocd    # type name of direcotry, automatically cd.
setopt correct   # spell-correct commands (with prompt)
setopt correctall # spell-correct everything
setopt histignoredups # prevents duplicates from being in history
# setopt histignorespace # avoid history by beginning line w/ space
# setopt ignorebraces # Turns off brace expansion (makes them easier to use)
setopt interactivecomments # Allows comments mid-line w/ #
# setopt globdots # files beginning with a . can be matched without specifying a dot.
# setopt cshjunkiequotes # Won't allow  unmatched quotes.
setopt rcquotes # include single quotes inline with '' 
# sunkeyboardhack # ignores an accidental single quote at the end of the line

# Grep coloring
export GREP_OPTIONS="--colour=auto"


#preexec () {
  #echo -ne "\ek${1%% *}\e\\"
#}


# Variables
export JohnTools='/Library/Frameworks/Python.framework/Versions/6.0.4/lib/python2.6/johntools.py'

# Directory aliases
alias weather='~/Experiments/weather_trunk/'
alias expserv='cd /Volumes/Experiments/john/Weather2'
alias nsf='cd ~/Documents/NSFapp'

# command aliases
alias mlab='matlab -nodisplay'
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
alias vim='vim -X'

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
export LS_COLORS=auto

#-------------------------------------------------------------
# tailoring 'less'
#-------------------------------------------------------------

alias more='less'
export PAGER=less
#export LESSCHARSET='latin1'   # I should maybe actually reconsider this
# export LESSOPEN='|/usr/bin/lesspipe.sh %s 2>&-'
# Use this if lesspipe.sh exists
export LESS='-i -N -w  -z-4 -g -e -M -X -F -R -P%t?f%f :stdin .?pb%pb\%:?lbLine %lb:?bbByte %bb:-...'

# Handy stuff for ipython
export LESS="-R" # Tells less to use raw input
export EDITOR=vim

# Make X11 defaults work.
#xrdb -merge ~/.Xdefaults &

# pip zsh completion start
function _pip_completion {
  local words cword
  read -Ac words
  read -cn cword
  reply=( $( COMP_WORDS="$words[*]" \ 
             COMP_CWORD=$(( cword-1 )) \
             PIP_AUTO_COMPLETE=1 $words[1] ) )
}
compctl -K _pip_completion pip
# pip zsh completion end

# Matlab
export PATH=$PATH:/opt/matlab/bin

# Cabal
export PATH=$PATH:/home/mcdon/.cabal/bin

# ADMB adjustments
export PATH=$PATH:/usr/local/admb/bin
export ADMB_HOME=/usr/local/admb/

# rstudio adjustments (this was a silly place to put this!
export PATH=$PATH:/usr/lib/rstudio/bin

# Autojump
source /etc/profile.d/autojump.zsh

export USERWM=`which xmonad`
export BROWSER=`which firefox`

# perl
export PERL_LOCAL_LIB_ROOT="/home/mcdon/perl5"
export PERL_MB_OPT="--install_base /home/mcdon/perl5"
export PERL_MM_OPT="INSTALL_BASE=/home/mcdon/perl5"
export PERL5LIB="/home/mcdon/perl5/lib/perl5/x86_64-linux-thread-multi:/home/mcdon/perl5/lib/perl5"
export PATH="/home/mcdon/perl5/bin:$PATH"

# ruby
export PATH="/home/mcdon/.gem/ruby/1.9.1/bin:$PATH"

# fasd
eval "$(fasd --init auto)"
alias v='f -e vim -b viminfo'
alias o='f -e xdg-open'
bindkey '^X^A' fasd-complete    # C-x C-a to do fasd-complete (fils and directories)
bindkey '^X^F' fasd-complete-f  # C-x C-f to do fasd-complete-f (only files)
bindkey '^X^D' fasd-complete-d  # C-x C-d to do fasd-complete-d (only directories)

