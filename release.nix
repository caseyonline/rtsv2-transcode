let
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
      rev = "b62ac1a4382826478a3e5e3293d42dc1c60e25c1";
      ref = "v2";
    };

  oxidizedPackages =
    builtins.fetchGit {
      name = "id3as-oxidized-packages";
      url = "git@github.com:id3as/oxidized.git";
      rev = "22e64b96fdf1849fff223082cd54e271acb1b9e8";
    };

  nixpkgs =
    import pinnedNix {
      overlays = [
        (import id3asPackages)
        (import oxidizedPackages)
      ];
    };

in

with nixpkgs;
stdenv.mkDerivation rec {
  pname = "rtsv2";
  version = "1.0";

  src = ./_build/default/rel/rtsv2;

  buildPhase = ''
  '';

  installPhase = ''
    if [ -n "$prefix" ]; then
        mkdir -p "$prefix"
    fi

    cp -r ./* $out/

    wrapProgram \
      $out/bin/start_iserf.sh \
      --prefix PATH : ${lib.makeBinPath [ serfdom ]}

    wrapProgram \
      $out/lib/rtsv2-*/priv/scripts/runMediaGateway.sh \
      --prefix PATH : ${lib.makeBinPath [ rtsv2-media-gateway ]}
  '';

  buildInputs = [

    # Needed for wrapProgram
    makeWrapper

    # Our nativedeps environment
    (id3as.nd-env.override {
      nd-quicksync-enabled = false;
      nd-bmd = null;
      nd-x264 = null;
      nd-x265 = null;
    })

    # Need to explicitly list dependencies
    # of erlang so that nix can find them
    # erlang itself isn't a dependency
    ncurses   # Ubuntu 18.04 has version 5, we need 6
    openssl   # Ubuntu 18.04 has version 1.1.0, we need 1.1.1

    # The Media Gateway
    rtsv2-media-gateway

    # Binaries we specifically want in the closure
    serfdom
  ];
}
