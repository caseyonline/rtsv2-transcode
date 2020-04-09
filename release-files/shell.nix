let
  erlangReleases = builtins.fetchTarball https://github.com/nixerl/nixpkgs-nixerl/archive/v1.0.2-devel.tar.gz;

  pinnedNix =
    builtins.fetchGit {
      name = "nixpkgs-pinned";
      url = "https://github.com/NixOS/nixpkgs.git";
      rev = "cc6cf0a96a627e678ffc996a8f9d1416200d6c81";
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
        (import id3asPackages)
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
    iproute
    id3as.nd-env
  ];
}
