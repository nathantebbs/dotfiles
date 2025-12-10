" Author: Nathan Tebbs
" File: .vimrc
" Modified: 20250116

" BASICS:
" Force vim not vi
let mapleader=","
set nocompatible

" Force split right
" set splitright

" Relative number
set relativenumber
set number

" Tabstop bullsplish
set tabstop=4
set softtabstop=4
set shiftwidth=4
set noexpandtab

" Autoindent
set autoindent

" netrw (":edit")
filetype plugin on

" OTHER:

" Scrolloff
set scrolloff=10

" Color column + textwidth (formatting)
set textwidth=79
set colorcolumn=+1

" Disable bg needed to support colorscheme bg, must be set before plugin is
let g:disable_bg = 1


" FINDING FILES:
" Search down into every subdirectory, tab-completion
set path+=**
set wildmenu


" NETRW:
" Tweaks
let g:netrw_banner=0
let g:netrw_browse_split=2
let g:netrw_altv=1
let g:netrw_liststyle=3

" BASIC REMAPS:
nnoremap <leader>s :so %<cr>
nnoremap <leader>w :w<cr>
nnoremap <leader>q :q<cr>
nnoremap <leader>pi :PlugInstall<cr>
nnoremap <leader>e :Rexplore<cr>
nnoremap <leader>R :so ~/.vimrc<cr>

" FZF
nnoremap <leader>fw :BLines<cr>
nnoremap <leader>fb :Buffers<cr>
nnoremap <leader>ff :Files<cr>
nnoremap <leader>fk :Maps<cr>


let g:highlightedyank_highlight_duration = 1000

" Python support
let g:python3_host_prog="/usr/bin/python3"



" PLUGINS:
call plug#begin()

Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'tpope/vim-surround'
Plug 'itchyny/lightline.vim'
Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }
Plug 'bfrg/vim-c-cpp-modern'
Plug 'vim-autoformat/vim-autoformat'
Plug 'prabirshrestha/vim-lsp'
Plug 'mattn/vim-lsp-settings'
Plug 'prabirshrestha/asyncomplete.vim'
Plug 'prabirshrestha/asyncomplete-lsp.vim'
Plug 'tribela/vim-transparent'
Plug 'jiangmiao/auto-pairs'
Plug 'sheerun/vim-polyglot'
Plug 'sakshamgupta05/vim-todo-highlight'

call plug#end()

" COLORSCHEME:
colorscheme habamax

" LIGHTLINE:
" Comment out the `colorscheme`.
let g:lightline = { 'colorscheme': 'molokai' }
" Enable lightline
set laststatus=2


" Modern c/c++
" Enable function highlighting (affects both C and C++ files)
let g:cpp_function_highlight = 1

" Enable highlighting of C++11 attributes
let g:cpp_attributes_highlight = 1

" Highlight struct/class member variables (affects both C and C++ files)
let g:cpp_member_highlight = 1

" Enable highlighting of type names in class, struct, union, enum, using, and
" concept declarations (affects both C and C++ files)
let g:cpp_type_name_highlight = 1

" Highlight operators (affects both C and C++ files)
let g:cpp_operator_highlight = 1

" Put all standard C and C++ keywords under Vim's highlight group 'Statement'
" (affects both C and C++ files)
let g:cpp_simple_highlight = 1


" AUTO FORMAT:
let g:python3_host_prog="/opt/homebrew/bin/python3"
au BufWrite * :Autoformat


let g:autoformat_autoindent = 0
let g:autoformat_retab = 0
let g:autoformat_remove_trailing_spaces = 0
let g:go_imports_autosave = 0
