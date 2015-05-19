{ stdenv, haskellPackages }:

let
  env = haskellPackages.ghcWithPackages (p: with p; [
    # Tools:
    cabal-install
    hlint

    # Libraries:
    aeson
    ansi-terminal
    base
    binary
    bytestring
    colour
    containers
    either
    exceptions
    haskeline
    http-client
    http-client-tls
    http-types
    mtl
    network
    network-uri
    old-locale
    optparse-applicative
    parsec
    process
    temporary
    terminfo-hs
    text
    text-binary
    time
    transformers
    unix
    xdg-basedir
    yaml
  ]);

in stdenv.mkDerivation rec {
  name = "vimeta";
  src = ./src;

  buildInputs = [ env ];

  buildPhase = ''
    ( HOME="$(mktemp -d)" # For cabal-install.
      if [ ! -d .cabal-sandbox ]; then
        cabal sandbox init
        cabal sandbox add-source vendor/themoviedb
        cabal sandbox add-source vendor/byline
        cabal install --only-dependencies
      fi

      cabal configure -fmaintainer
      cabal build || exit 1
    ) && hlint src
  '';

  installPhase = ''
  '';

  shellHook = ''
    export NIX_GHC="${env}/bin/ghc"
    export NIX_GHCPKG="${env}/bin/ghc-pkg"
    export NIX_GHC_DOCDIR="${env}/share/doc/ghc/html"
    export NIX_GHC_LIBDIR=$( $NIX_GHC --print-libdir )
  '';
}
