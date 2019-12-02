let
  erlangReleases = builtins.fetchTarball https://github.com/nixerl/nixpkgs-nixerl/archive/v1.0.2-devel.tar.gz;

  purerlReleases = builtins.fetchTarball https://github.com/purerl/nixpkgs-purerl/archive/master.tar.gz;

  id3asPackages =
    builtins.fetchGit {
      name = "id3as-packages";
      url = "git@github.com:id3as/nixpkgs-private.git";
      rev = "d6b5a626f042182ef91ef360fe80c66459dd695e";
    };

  nixpkgs =
    import <nixpkgs> {
      overlays = [
        (import erlangReleases)
        (import purerlReleases)
        (import id3asPackages)
      ];
    };

in

with nixpkgs;

mkShell {
  buildInputs = with pkgs; [

    nixerl.erlang-22-1-8.erlang
    nixerl.erlang-22-1-8.rebar3

    # Needed for UI build
    nodejs

    # Our nativedeps environment
    id3as.nd-env

    # Purescript
    purerl.purerl-0-13-5

    # Purescript extras
    id3as.spago-0-12-1-0
    id3as.dhall-json-1-5-0

    # Needed by something purescript-y - hopefully A/S can pinpoint what...
    jq
  ];
}
