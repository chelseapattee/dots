#! /bin/bash

mkdir "${HOME}/virtualenvs"

ln -sf "${PWD}/vimrc" "${HOME}/.vimrc"
ln -sf "${PWD}/vim" "$HOME/.vim"
ln -sf "${PWD}/emacs" "${HOME}/.emacs.d"
ln -sf "${PWD}/jupyter" "${HOME}/.jupyter"
ln -sf "${PWD}/dircolors.ansi-dark" "${HOME}/.dircolors"
ln -sf "${PWD}/bash/bash_profile" "${HOME}/.bash_profile"
ln -sf "${PWD}/bash/bashrc" "${HOME}/.bashrc"
ln -sf "${PWD}/inputrc" "${HOME}/.inputrc"

mkdir -p "${HOME}/.ssh"
ln -sf "${PWD}/sshrc" "${HOME}/.ssh/rc"

if [ "$(uname -s)" = "Darwin" ]; then
  ln -sf "${PWD}/tmux.conf.osx" "${HOME}/.tmux.conf"
else
  ln -sf "${PWD}/tmux.conf" "$HOME/.tmux.conf"
fi
