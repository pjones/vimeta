{ sources ? import ./nix/sources.nix
, pkgs ? import sources.nixpkgs {}
, nix-hs ? import sources.nix-hs { inherit pkgs; }
, byline ? sources.byline
, themoviedb ? sources.themoviedb
, ghcide ? sources.ghcide-nix
, ormolu ? sources.ormolu
}:

nix-hs {
  cabal = ./vimeta.cabal;

  buildInputs = with pkgs; [ atomicparsley ];

  overrides = lib: self: super: with lib; {
    byline = import byline { inherit (lib) pkgs; };
    themoviedb = import themoviedb { inherit (lib) pkgs; };
    ghcide = import ghcide {};

    ormolu = (import ormolu {
      inherit (lib) pkgs;
      ormoluCompiler = lib.compilerName;
    }).ormolu;

    optparse-applicative =
      if super ? optparse-applicative_0_15_0_0
        then super.optparse-applicative_0_15_0_0
        else optparse-applicative;
  };
}
