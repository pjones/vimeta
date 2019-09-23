{ pkgs ? import <nixpkgs> { }
}:

let
  # Extra system dependencies needed:
  extraPackages = with pkgs; [ atomicparsley ];

  # Grab the latest version of some dependencies:
  themoviedb = fetchGit "https://code.devalot.com/open/themoviedb.git";
  byline = fetchGit "https://code.devalot.com/open/byline.git";

  # Helpful if you want to override any Haskell packages:
  overrides = self: super: {
    http-client = if super ? http-client_0_6_2
      then super.http-client_0_6_2
      else super.http-client;

    byline = import "${byline}/default.nix" { inherit pkgs; };
    themoviedb = import "${themoviedb}/default.nix" { inherit pkgs; };
  };

  # Apply the overrides from above:
  haskell = pkgs.haskellPackages.override (orig: {
    overrides = pkgs.lib.composeExtensions
      (orig.overrides or (_: _: {})) overrides; });
in

# Load the local nix file and use the overrides from above:
haskell.callPackage ./vimeta.nix {
  mkDerivation = { buildTools ? []
                 , ...
                 }@args:
    haskell.mkDerivation (args // {
      buildTools = buildTools ++ extraPackages;
    });
}
