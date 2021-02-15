ORANGE="\[\033[0;33m\]"
BLUE="\[\033[0;34m\]"
WHITE="\[\033[00m\]"

function function_exists() {
  declare -f -F $1 > /dev/null
  return $?
}

function get_git_branch {
  git rev-parse --symbolic-full-name --abbrev-ref HEAD 2> /dev/null
}

function set_virtualenv () {
  if [ -z "$VIRTUAL_ENV" ]; then
      PYTHON_VIRTUALENV=""
  else
      PYTHON_VIRTUALENV="${BLUE}`basename \"$VIRTUAL_ENV\"`${COLOR_NONE}"
  fi
}

function set_ps1 {
  local ps1="[" # ${ORANGE}\u${WHITE}:${BLUE}\h${WHITE}"
  local git_branch=$(get_git_branch)
  set_virtualenv

  # TODO(benr): known clowntown, hoist this up into a `get_virtualenv_ps1_component`
  [ ! -z "$PYTHON_VIRTUALENV" ] && ps1="${ps1} ${ORANGE}venv${WHITE}:${BLUE}${PYTHON_VIRTUALENV}${WHITE}"

  NODE=$(which node)
  NODE_VERSION=$(${NODE} --version)
  if [ -x "${NODE}" ]; then
    ps1="${ps1} ${ORANGE}node${WHITE}:${BLUE}${NODE_VERSION}${WHITE}"
  fi

  if [ ! -z $git_branch ]; then
    ps1="${ps1} ${ORANGE}git${WHITE}:${BLUE}${git_branch}${WHITE}"
  fi

  PS1="${ps1} ${ORANGE}\w${WHITE} ] "
}

export PROMPT_COMMAND="history -a; set_ps1"
