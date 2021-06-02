{ sources ? import ./nix/sources.nix
, pkgs ? import sources.nixpkgs { }
, nix-hs ? import sources.nix-hs { inherit pkgs; }
, ghc ? "default"
}:

nix-hs {
  cabal = ./vimeta.cabal;
  buildInputs = with pkgs; [ atomicparsley ];
  compiler = ghc;

  overrides = lib: self: super: {
    attoparsec = super.attoparsec_0_14_1;
    cryptonite = super.cryptonite_0_29;
    haskeline = super.haskeline_0_8_1_2;
    memory = super.memory_0_16_0;
    relude = super.relude_1_0_0_1;

    # https://github.com/snoyberg/mono-traversable/issues/192
    mono-traversable = lib.dontCheck super.mono-traversable;

    byline =
      (import sources.byline {
        inherit (lib) pkgs;
        inherit ghc;
      });

    themoviedb =
      (import sources.themoviedb {
        inherit (lib) pkgs;
        inherit ghc;
      });
  };
}
