#!/bin/zsh

set -e

if [[ ! -f ~/.vim/autoload/pathogen.vim ]]; then
   mkdir -p ~/.vim/autoload ~/.vim/bundle ~/.vim/undo ~/.vim/swap ~/.vim/backup
   curl -LSso ~/.vim/autoload/pathogen.vim https://tpo.pe/pathogen.vim
fi

if [[ ! -d ~/.vim/bundle/vim-sensible ]]; then
    git clone https://github.com/tpope/vim-sensible ~/.vim/bundle/vim-sensible
fi

if [[ ! -d ~/.vim/bundle/vim-airline ]]; then
    git clone https://github.com/bling/vim-airline ~/.vim/bundle/vim-airline
fi

if [[ ! -d ~/.vim/bundle/vim-easymotion ]]; then
    git clone https://github.com/Lokaltog/vim-easymotion ~/.vim/bundle/vim-easymotion
fi

if [[ ! -d ~/.fzf ]]; then
    git clone https://github.com/junegunn/fzf.git ~/.fzf
    ~/.fzf/install
fi

BREW_PATH=$(which brew)
if [[ ! -f $BREW_PATH ]]; then
    ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
fi

if [[ ! -f ~/.zshrc-cask-p ]]; then
    apps=(dropbox google-chrome firefox slack iterm2 vlc caffeine spectacle spotify dash quicksilver emacs authy dashlane)
    brew cask install --appdir="/Applications" ${apps[@]}
    brew install fzf yarn node openssh font-fira-code hub
    touch ~/.zshrc-cask-p
fi
