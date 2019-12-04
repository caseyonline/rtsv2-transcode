let
  nixpkgs =
    import <nixpkgs> {} ;
in

with nixpkgs;

mkShell {
  buildInputs = with pkgs; [
    purescript
  ];
}
