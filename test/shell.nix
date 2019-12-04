let
  id3asPackages =
    builtins.fetchGit {
      name = "id3as-packages";
      url = "git@github.com:id3as/nixpkgs-private.git";
      rev = "d6b5a626f042182ef91ef360fe80c66459dd695e";
    };

  nixpkgs =
    import <nixpkgs> {
      overlays = [
        (import id3asPackages)
      ];
    } ;
in

with nixpkgs;

mkShell {
  buildInputs = with pkgs; [
    nodejs
    purescript
    id3as.spago-0-12-1-0
    id3as.dhall-json-1-5-0
    jq
  ];
}
