let
  erlangReleases = builtins.fetchTarball https://github.com/nixerl/nixpkgs-nixerl/archive/v1.0.4-devel.tar.gz;

  pinnedNix =
    builtins.fetchGit {
      name = "nixpkgs-pinned";
      url = "https://github.com/NixOS/nixpkgs.git";
      rev = "cc6cf0a96a627e678ffc996a8f9d1416200d6c81";
    };

  purerlReleases =
    builtins.fetchGit {
      url = "https://github.com/purerl/nixpkgs-purerl.git";
      ref = "master";
      rev = "5da0a433bcefe607e0bd182b79b220af980a4c78";
    };
  id3asPackages =
    builtins.fetchGit {
      name = "id3as-packages";
      url = "git@github.com:id3as/nixpkgs-private.git";
      rev = "cb044716e048364a028f9fb17b06e12058d68c97";
    };

  nixpkgs =
    import pinnedNix {
      overlays = [
        (import erlangReleases)
        (import purerlReleases)
        (import id3asPackages)
      ];
    };

in

with nixpkgs;

let
    inherit (stdenv.lib) optionals;
in

mkShell {
  buildInputs = with pkgs; [

    # Use GNU coreutils - so that if we're building on OSX, we get sensible versions
    # of things like readlink
    coreutils

    # Bash on macOS is ancient
    bash

    tmux

    nixerl.erlang-22-3.erlang
    nixerl.erlang-22-3.rebar3

    # Needed for UI build
    nodejs
    nodePackages.webpack
    nodePackages.webpack-cli

    # Our nativedeps environment
    id3as.nd-env

    # Purescript - we use a specific version rather than
    # whatever the latest is exposed via nixpkgs
    id3as.purescript-0-13-6

    # Purescript extras
    id3as.spago-0-12-1-0
    id3as.dhall-json-1-5-0

    # Purerl backend for purescript
    purerl.purerl-0-0-5

    # Needed by something purescript-y - hopefully A/S can pinpoint what...
    jq
    serfdom
  ] ++ optionals stdenv.isLinux [ iproute
                                ]
;
}
