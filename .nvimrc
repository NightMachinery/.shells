nnoremap <Space> <nop>
:let mapleader = " "

set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'

" The following are examples of different formats supported.
" Keep Plugin commands between vundle#begin/end.
" plugin on GitHub repo
Plugin 'tpope/vim-fugitive'
" plugin from http://vim-scripts.org/vim/scripts.html
" Plugin 'L9'
" Git plugin not hosted on GitHub
"Plugin 'git://git.wincent.com/command-t.git'
" git repos on your local machine (i.e. when working on your own plugin)
"Plugin 'file:///home/gmarik/path/to/plugin'
" The sparkup vim script is in a subdirectory of this repo called vim.
" Pass the path to set the runtimepath properly.
"Plugin 'rstacruz/sparkup', {'rtp': 'vim/'}
" Install L9 and avoid a Naming conflict if youve already installed a
" different version somewhere else.
" Plugin 'ascenator/L9', {'name': 'newL9'}
Plugin 'udalov/kotlin-vim'
Plugin 'git://github.com/tpope/vim-unimpaired.git'
Plugin 'tpope/vim-surround'
Plugin 'scrooloose/syntastic'
Plugin 'https://github.com/ctrlpvim/ctrlp.vim.git'
Plugin 'altercation/vim-colors-solarized'
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
" Plugin 'valloric/youcompleteme'
Plugin 'easymotion/vim-easymotion'
Plugin 'tpope/vim-repeat'
Plugin 'rhysd/clever-f.vim'
Plugin 'justinmk/vim-sneak'
Plugin 'scrooloose/nerdtree'
Plugin 'iCyMind/NeoSolarized'
Plugin 'scrooloose/nerdcommenter'
Plugin 'NLKNguyen/papercolor-theme'

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required
" To ignore plugin indent changes, instead use:
"filetype plugin on
"
" Brief help
" :PluginList       - lists configured plugins
" :PluginInstall    - installs plugins; append `!` to update or just :PluginUpdate
" :PluginSearch foo - searches for foo; append `!` to refresh local cache
" :PluginClean      - confirms removal of unused plugins; append `!` to auto-approve removal
"
" see :h vundle for more details or wiki for FAQ
" Put your non-Plugin stuff after this line
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

syntax enable
set background=light
inoremap ii <Esc>
" inoremap jj <BS>

set number "Shows line numbers

colorscheme solarized
call togglebg#map("<F5>")
let g:airline_theme='solarized'
if has("nvim") && $TERM_PROGRAM == "iTerm.app"
  " has true colour support and is nvim
  set termguicolors
  let $NVIM_TUI_ENABLE_TRUE_COLOR=1
  colorscheme NeoSolarized
elseif $TERM_PROGRAM == "iTerm.app"
  " has true colour support but is using std vim
  set termguicolors
  colorscheme NeoSolarized
else
  "set termguicolors
  set t_Co=256   " This is may or may not needed.
  set background=light
  colorscheme PaperColor
  let g:airline_theme='papercolor'
endif

let g:ycm_server_keep_logfiles = 1
let g:ycm_server_log_level = 'debug'
au FileType crontab setlocal bkc=yes

map <Leader>n :NERDTreeFind<CR>
autocmd BufEnter * lcd %:p:h "  configure Vim so that it sets the working directory to the current file's directory
