let g:sneak#label = 1

let g:ale_linters = {
\   'typescript': ['tsserver'],
\}

let g:airline_section_b = ''
let g:airline_section_x = ''
let g:airline_section_y = ''
let g:airline_section_z = ''

let g:airline_theme = 'tender'

call plug#begin('~/.local/share/nvim/plugged')
Plug 'https://github.com/tpope/vim-sensible.git'
Plug 'https://github.com/vim-airline/vim-airline.git'
Plug 'tpope/vim-surround'
Plug 'airblade/vim-gitgutter'
Plug 'justinmk/vim-sneak'
Plug 'jacoborus/tender.vim'
Plug 'w0rp/ale'
Plug '~/.fzf'
Plug 'junegunn/fzf.vim'
Plug 'ap/vim-css-color'

" typescript
Plug 'leafgarland/typescript-vim'
call plug#end()

colorscheme tender

" http://nvie.com/posts/how-i-boosted-my-vim/
let mapleader=","

set termguicolors
set hidden " closed buffers
set nowrap
set number " of the line variety
set showmatch " parentheses
set title " change terminal's title
set ch=2 " command-line height
set laststatus=2 " always show status bar
set wrapscan " wrap search
set backupdir=/tmp

set ignorecase
set smartcase
set hlsearch
set gdefault
set showmatch
set hlsearch

" Python/Perl regex syntax
nnoremap / /\v
vnoremap / /\v
nnoremap <leader><space> :noh<cr> " unhighlight

set tabstop=4
set shiftwidth=4
set softtabstop=4
set copyindent " new lines copy previous lines' indent
set shiftround " < and >
set expandtab
set relativenumber " relative line number numbering for fast moves
set listchars=tab:>-,trail:~,extends:>,precedes:<

set history=1000
set undolevels=1000

set backupdir=~/.vim/backup//
set undodir=~/.vim/undo//
set directory=~/.vim/swap//

noremap ; :
inoremap jk <ESC> " jj to go back to normal mode
inoremap kj <ESC> " jj to go back to normal mode

nnoremap <M-j> :m .+1<CR>
nnoremap <M-k> :m .-2<CR>
inoremap <M-j> <Esc>:m .+1<CR>gi
inoremap <M-k> <Esc>:m .-2<CR>gi
vnoremap <M-j> :m '>+1<CR>gv
vnoremap <M-k> :m '<-2<CR>gv

for i in range(65,90) + range(97,122)
  let c = nr2char(i)
  exec "imap \e".c." <M-".c.">"
  exec "map \e".c." <M-".c.">"
endfor

autocmd FileType haskell setlocal shiftwidth=2

set vb t_vb=
autocmd GUIEnter * set vb t_vb=

set inccommand=nosplit
