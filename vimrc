" This must be first, because it changes other options as a side effect.
set nocompatible

filetype off

source ~/.vim/bundles.vim

filetype plugin indent on

set tabstop=8
set expandtab
set softtabstop=4
set shiftwidth=4
set autoindent

" autocmd VimResized * wincmd =

" allow backspacing over everything in insert mode
set backspace=indent,eol,start

if has("vms")
  set nobackup		" do not keep a backup file, use versions instead
else
  set backup		" keep a backup file
endif
set history=80		" keep 50 lines of command line history
set ruler		" show the cursor position all the time
set showcmd		" display incomplete commands
set incsearch		" do incremental searching

"Scroll off... how many lines to keep above or below the cursor.
set so=6

" For Win32 GUI: remove 't' flag from 'guioptions': no tearoff menu entries
" let &guioptions = substitute(&guioptions, "t", "", "g")
" Use console dialogs:
" set guioptions+=c
" Remove scrollbar:
" let &guioptions = substitute(&guioptions, "r", "", "g")
" Reomve default menus:
" let did_install_default_menus = 1

" Don't use Ex mode, use Q for formatting
" map Q gq
set formatoptions=tcroq

" CTRL-U in insert mode deletes a lot.  Use CTRL-G u to first break undo,
" so that you can undo CTRL-U after inserting a line break.
inoremap <C-U> <C-G>u<C-U>

" In many terminal emulators the mouse works just fine, thus enable it.
if has('mouse')
  set mouse=a
endif

" Switch syntax highlighting on, when the terminal has colors
" Also switch on highlighting the last used search pattern.
if &t_Co > 2 || has("gui_running")
  syntax on
  set hlsearch
endif

" Only do this part when compiled with support for autocommands.
if has("autocmd")

  " Enable file type detection.
  " Use the default filetype settings, so that mail gets 'tw' set to 72,
  " 'cindent' is on in C files, etc.
  " Also load indent files, to automatically do language-dependent indenting.

  " Put these in an autocmd group, so that we can delete them easily.
  augroup vimrcEx
  au!

  " For all text files set 'textwidth' to 78 characters.
  autocmd FileType text setlocal textwidth=78

  " When editing a file, always jump to the last known cursor position.
  " Don't do it when the position is invalid or when inside an event handler
  " (happens when dropping a file on gvim).
  " Also don't do it when the mark is in the first line, that is the default
  " position when opening a file.
  autocmd BufReadPost *
    \ if line("'\"") > 1 && line("'\"") <= line("$") |
    \   exe "normal! g`\"" |
    \ endif

  augroup END

else

  set autoindent		" always set autoindenting on

endif " has("autocmd")

" Convenient command to see the difference between the current buffer and the
" file it was loaded from, thus the changes you made.
" Only define it when not defined already.
if !exists(":DiffOrig")
  command DiffOrig vert new | set bt=nofile | r # | 0d_ | diffthis
		  \ | wincmd p | diffthis
endif

set modeline

" Line numbering:
set number
nnoremap <F2> :set nonumber!<CR>:set foldcolumn=0<CR>

" Generate list of TODOs
imap \= <Esc>:grep --exclude=*~ TODO * <CR> :copen <CR>
map \= :grep --exclude=*~ TODO * <CR> :copen <CR>

" Map 'cd' alone to change directory to current path.
map ,cd :cd %:p:h<CR>:pwd<CR>

" Save folding
au BufWinLeave *.* mkview
au BufWinEnter *.* loadview

" Auto-load this file on save:
au BufLeave ~/.vimrc :source ~/.vimrc


" Latex Stuff.

" REQUIRED. This makes vim invoke Latex-Suite when you open a tex file.
" filetype plugin on

" IMPORTANT: grep will sometimes skip displaying the file name if you
" search in a singe file. This will confuse Latex-Suite. Set your grep
" program to always generate a file-name.
set grepprg=grep\ -nH\ $*

" Turn on Wildmenu (wild!)
set wildmenu 

" Persistent undo!
set undofile
set undodir=~/.undo

" Smart search
set ignorecase
set smartcase

" Set options for miniBufExplorer
let g:miniBufExplMapWindowNavVim = 1
let g:miniBufExplMapWindowNavArrows = 1 
let g:miniBufExplMapCTabSwitchBufs = 1 
let g:miniBufExplModSelTarget = 1 

" OPTIONAL: Starting with Vim 7, the filetype of empty .tex files defaults
" to
" 'plaintex' instead of 'tex', which results in vim-latex not being loaded.
" The following changes the default filetype back to 'tex':
let g:tex_flavor='latex'
autocmd FileType python set omnifunc=pysmell#Complete

" For vimrplugin
" let vimrplugin_term = "urxvt"  " By default probably uses aterm.
" let vimrplugin_term = "gnome-terminal"  " By default probably uses aterm.
" let vimrplugin_term_cmd = "urxvt -tint white -sh 18"  " By default probably uses aterm.
let vimrplugin_underscore = 1  " _  generates <-, __ generates _

"if v:progname =~? "mvim"
  " let vimrplugin_term_cmd = "/Applications/Utilities/iTerm.app/Contents/MacOS/iTerm -t R"
  " let vimrplugin_term_cmd = "/Applications/Utilities/Terminal.app/Contents/MacOS/terminal -t R"
"endif
let vimrplugin_conqueplugin = 0
" Trying very hard to use tmux:
let vimrplugin_tmux = 1
let g:ScreenImpl = 'Tmux'
let g:vimrplugin_tmux = 1
let vimrplugin_vimpager="horizontal"


" For svndiff
let g:svndiff_autoupdate=1
hi DiffAdd      ctermfg=0 ctermbg=2 guibg='green'
hi DiffDelete   ctermfg=0 ctermbg=1 guibg='red'
hi DiffChange   ctermfg=0 ctermbg=3 guibg='yellow' 

" Vundle stuff:
source ~/.vim/bundles.vim

" vim-r plugin.
let vimrplugin_conqueplugin = 0
let vimrplugin_vimpager="horizontal"
let r_syntax_folding = 1

" For pathogen
"call pathogen#runtime_append_all_bundles()

" For FuzzyFinder
nmap ,f :FufFileWithCurrentBufferDir<CR>
nmap ,b :FufBuffer<CR>
nmap ,t :FufTaggedFile<CR>  " Not sure how to get tags for non-c++ things
nmap ,c :FufChangeList<CR>

" For conque
let g:ConqueTerm_SendVisKey = '`'

" for taglist (can't find exuberant ctags)
let Tlist_Ctags_Cmd = '/usr/local/bin/ctags'

colorscheme molokai

