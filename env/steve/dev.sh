# So that id3as_media doesn't prevent loading NIFs under emacs
export IGNORE_EMACS=

# History is nice in shells, let's have that
export ERL_AFLAGS="-kernel shell_history enabled"

# Use nix set-up
source_env ../common/nix.sh
