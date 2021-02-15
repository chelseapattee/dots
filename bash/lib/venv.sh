declare -a VENVS_PREF_LIST=("dev")

for i in ${VENVS_PREF_LIST[@]}; do
  VENV_FILE="${HOME}/virtualenvs/${i}/bin/activate"
  if [ -f "${VENV_FILE}" ]; then
    source "${VENV_FILE}"
    break
  fi
done
