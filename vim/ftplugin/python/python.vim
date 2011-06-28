set tabstop=8
set expandtab
set softtabstop=4
set shiftwidth=4
set autoindent

let g:pydiction_location="~/.vim/pydiction/complete-dict"

" Somehow, this thing here makes ( not end a word
" set complete+=k~/.vim/syntax/python.vim isk+=.,(

filetype plugin indent on

" Execute file being edited with <Shift> + e:
map <buffer> <S-x> :w<CR>:!/Library/Frameworks/Python.framework/Versions/Current/bin//python % <CR>
" Insert a vimpdb trace point.
" map <buffer> \t oimport vimpdb; vimpdb.set_trace()<esc>
" assigns trace to register t.
let @t = "import vimpdb; vimpdb.set_trace()"
" let @d = "import ipdb; ipdb.set_trace()"

python << EOF
import os
import sys
import vim
for p in sys.path:
    # Add each directory in sys.path, if it exists.
    if os.path.isdir(p):
        # Command 'set' needs backslash before each space.
        vim.command(r"set path+=%s" % (p.replace(" ", r"\ ")))
EOF



source ~/.vim/ftplugin/python/jpythonfold.vim
filetype detect
