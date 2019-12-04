id3as::script_path() {

  # Yes, this can all be done on one line, but this way I'll understand it when
  # I come back to it :P

  # BASH
  if [[ -n "${BASH_SOURCE[0]}" ]]; then
    echo "${BASH_SOURCE[0]}"
    return
  fi

  # ZSH
  # shellcheck disable=2154
  echo "${(%):-%x}"
  return
}

id3as::establish_environment() {
  local this_dir
  local proj_dir
  this_dir=$(dirname "$(id3as::script_path)")
  proj_dir=$(readlink --canonicalize "${this_dir}/..")

  printf "Running from %s, the project directory is %s...\n" "${this_dir}" "${proj_dir}"

  printf "Sourcing id3as_media...\n"
  export LD_LIBRARY_PATH="${proj_dir}/_build/default/lib/id3as_media/priv/:$LD_LIBRARY_PATH"

  if [[ -f "${this_dir}/${USER}/dev.sh" ]]; then
    source_env "${this_dir}/${USER}/dev.sh"
  fi

  # Use nix set-up
  source_env common/nix.sh
}

id3as::establish_environment

unset -f id3as::establish_environment
unset -f id3as::script_path
