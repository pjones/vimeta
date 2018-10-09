{ pkgs ? (import <nixpkgs> {}).pkgs }:

let
  # List any extra packages you want available while your package is
  # building or while in a nix shell:
  extraPackages = with pkgs; [ atomicparsley ];

  # Grab the latest TheMovieDB package:
  themoviedb = fetchGit "git://git.devalot.com/themoviedb.git";

  # Helpful if you want to override any Haskell packages:
  haskell = pkgs.haskellPackages.override (orig: {
    overrides = pkgs.lib.composeExtensions
                  (orig.overrides or (_: _: {}))
                  (self: super: with pkgs.haskell.lib; {
                    themoviedb = self.callPackage "${themoviedb}/themoviedb.nix" { };
                  });
    });
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
