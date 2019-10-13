{ pkgs ? import <nixpkgs> { }
}:

let
  nix-hs-src = fetchGit {
    url = "https://code.devalot.com/open/nix-hs.git";
    rev = "a2b666faf8cb3c6f769655dfb36f4695f78bc3c3";
  };

  nix-hs = import "${nix-hs-src}/default.nix" { inherit pkgs; };

in nix-hs {
  cabal = ./vimeta.cabal;

  buildInputs = with pkgs; [ atomicparsley ];

  overrides = lib: self: super: with lib; {
    byline = lib.fetchGit {
      url = "https://code.devalot.com/open/byline.git";
      rev = "f2e6eb1926873433129d3103efe11090b3f1b514";
    };

    themoviedb = lib.fetchGit {
      url = "https://code.devalot.com/open/themoviedb.git";
      rev = "4aeb946d5b25754eab5daaab1ba0f269f03a7c59";
    };

    optparse-applicative =
      if super ? optparse-applicative_0_15_0_0
        then super.optparse-applicative_0_15_0_0
        else optparse-applicative;
  };
}
