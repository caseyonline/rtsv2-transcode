let
  erlangReleases = builtins.fetchTarball https://github.com/nixerl/nixpkgs-nixerl/archive/v1.0.2-devel.tar.gz;

  nixpkgs =
    import <nixpkgs> {
      overlays = [
        (import erlangReleases)
      ];
    };

in

with nixpkgs;

mkShell {
  buildInputs = with pkgs; [
    coreutils
    bash
    nixerl.erlang-22-1-8.erlang
    serfdom
  ];
}
