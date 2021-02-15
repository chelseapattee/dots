export CLICOLOR=1

if [ "$(uname -s)" != "Darwin" ]; then
  eval "$(dircolors /home/${USER}/.dircolors)"
  alias ls='ls --color=always'
else
  eval "$(gdircolors /Users/${USER}/.dircolors)"
  alias ls='gls --color=always'
fi
