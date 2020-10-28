{ sources ? import ./nix/sources.nix
, nixpkgs ? "nixpkgs"
, pkgs ? import sources.${nixpkgs} { }
, nix-hs ? import sources.nix-hs { inherit pkgs; }
, ghc ? "default"
}:

nix-hs {
  cabal = ./vimeta.cabal;
  buildInputs = with pkgs; [ atomicparsley ];
  compiler = ghc;

  overrides = lib: self: super: {
    byline = (import sources.byline {
      inherit (lib) pkgs;
      inherit ghc;
    });

    themoviedb =
      (import sources.themoviedb {
        inherit (lib) pkgs;
        inherit ghc;
      });

    aeson =
      if super ? aeson_1_5_2_0
      then super.aeson_1_5_2_0
      else super.aeson;

    ansi-terminal =
      if super ? ansi-terminal_0_11
      then super.ansi-terminal_0_11
      else super.ansi-terminal;

    http-client =
      if super ? http-client_0_7_2_1
      then super.http-client_0_7_2_1
      else super.http-client;

    haskeline = lib.dontCheck (
      if super ? haskeline_0_8_1_0
      then lib.dontCheck super.haskeline_0_8_1_0
      else super.haskeline
    );

    optparse-applicative =
      if super ? optparse-applicative_0_16_0_0
      then super.optparse-applicative_0_16_0_0
      else super.optparse-applicative;

    relude =
      if super ? relude_0_6_0_0
      then super.relude_0_6_0_0 # NixOS 20.03
      else super.relude;
  };
}
