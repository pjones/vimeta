# This is currently a Nix expression for building with cabal but I
# plan on changing it so it just builds with GNU Make.  That way all
# of my sandboxing will work correctly.
{ pkgs ? (import <nixpkgs> {}) }:

let haskellPackages = pkgs.haskellPackages; in

haskellPackages.cabal.mkDerivation (self: {
  pname = "vimeta";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;

  buildTools = with pkgs; [
    haskellPackages.ghc
    haskellPackages.cabalInstall
  ];

  buildDepends = with pkgs; [
    # Haskell packages.
    haskellPackages.mtl

    # Libraries needed by Haskell packages:
    zlib
  ];

  meta = with self.stdenv.lib; {
    homepage = http://github.com/pjones/vimeta;
    description = "";
    license = licenses.gpl3;
    platforms = self.ghc.meta.platforms;
  };
})
