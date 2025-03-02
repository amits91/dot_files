" =============================================================================
" Updated vimrc using vim-plug as the sole plugin manager
" =============================================================================

" --- Basic settings ---
set nocompatible              " Disable compatibility with vi for improved Vim features
filetype off                  " Temporarily disable filetype detection for plugin setup

" --- Plugin Manager: vim-plug ---
" Ensure vim-plug is installed in ~/.vim/autoload/plug.vim
" (To install vim-plug, run:
"  curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
"       https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim)
call plug#begin('~/.vim/plugged')

" List of plugins to install using vim-plug
" XML editing plugin
Plug 'othree/xml.vim'

" Enhance text editing: smart case replacement, abbreviation enhancements
Plug 'tpope/vim-abolish'
Plug 'tpope/vim-repeat'

" CSV handling plugin
Plug 'chrisbra/csv.vim'

" Uncomment below plugins if desired
" Plug 'dense-analysis/ale'          " Asynchronous linting and fixing
" Plug 'neoclide/coc.nvim', {'branch': 'release'}  " LSP-based completion (if you decide to switch later)
" Plug 'Valloric/YouCompleteMe'        " YouCompleteMe (requires compilation)
" Plug 'rip-rip/clang_complete'        " clang_complete integration

call plug#end()             " End vim-plug block

" Re-enable filetype detection and plugins
filetype plugin indent on

" --- General Appearance & Behavior ---
colorscheme default         " Use the default colorscheme (customize if desired)
if has('mouse')
    set mouse=a             " Enable mouse support in all modes
endif

" --- Toggle Mouse Mode (F12) ---
" F12 toggles between 'mouse=a' (Vim mode) and disabled (xterm mode)
nnoremap <silent><F12> :let &mouse = (&mouse == "a" ? "" : "a")<CR>:call ShowMouseMode()<CR>
inoremap <silent><F12> <ESC>:let &mouse = (&mouse == "a" ? "" : "a")<CR>:call ShowMouseMode()<CR>
function! ShowMouseMode()
    if (&mouse == 'a')
        echo "mouse-vim"
    else
        echo "mouse-xterm"
    endif
endfunction

" --- Editing & Search Settings ---
set backspace=indent,eol,start   " Make backspace work naturally
set ruler                        " Show cursor position in the status line
set showcmd                      " Show incomplete commands in the corner
set hlsearch                     " Highlight all search matches
set nowrapscan                   " Do not wrap searches around end of file
set ignorecase                   " Case-insensitive search
set smartcase                    " But make search case-sensitive if uppercase letters are used
set incsearch                    " Incremental search as you type

" --- Indentation Settings ---
set autoindent                   " Continue indenting on new lines
set smartindent                  " Enable smart indenting for C/C++ etc.
set tabstop=4                    " A tab equals 4 spaces
set shiftwidth=4                 " Indent with 4 spaces
set expandtab                    " Convert tabs to spaces

" --- Syntax & Visual Settings ---
syntax on                      " Enable syntax highlighting
set t_Co=256                   " Support 256 colors
set showmatch                  " Highlight matching brackets
set scrolloff=5                " Keep at least 5 lines visible above and below the cursor
set number                     " Enable line numbers

" --- Leader Key ---
let mapleader = "\\"           " Set leader key to backslash
nmap , <leader>              " Map comma as an additional leader if desired

" --- Filetype and Autocommand Settings ---
" Enable filetype plugins and indenting
filetype plugin on

if has("autocmd")
    " Jump to the last cursor position when reopening files
    autocmd BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g`\"" | endif
endif

" --- Custom Key Mappings & Commands ---
map Q :q<CR>                  " Press Q to quit Vim
" Save file mappings
nmap <leader>w :w<CR>
inoremap <leader>w <ESC>:w<CR>

" --- Window Navigation ---
nmap <silent> <leader><Up> :wincmd k<CR>
nmap <silent> <leader><Down> :wincmd j<CR>
nmap <silent> <leader><Left> :wincmd h<CR>
nmap <silent> <leader><Right> :wincmd l<CR>

" --- Miscellaneous Mappings ---
" Toggle line numbers
nmap <leader>nu :set nu!<CR>
" Toggle scrollbind
nmap <F11> :set scb!<CR>

" --- End of Vim Settings ---
" (Add any additional customizations below as needed)
