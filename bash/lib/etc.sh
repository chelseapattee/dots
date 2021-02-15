# only mess with the tty settings in an interactive shell
[[ $- == *i* ]] && stty -ixon -ixoff

# TODO(benr): debug duplicate entries in path
export PATH="${HOME}/bin:${HOME}/.cask/bin:${PATH}"

export EDITOR=vi

# ignore and erase duplicate entries from command history
export HISTCONTROL=ignoreboth:erasedups

# this is important for solarized-16
alias emacs="TERM=xterm emacs"

cd "${HOME}" # this is nice for new tmux splits when pwd is a symlink
