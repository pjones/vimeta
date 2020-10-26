{ ghc ? "default"
}:
let
  sources = import nix/sources.nix;
  pkgs = import sources.nixpkgs { };
  lib = pkgs.lib;

  nix_path = {
    nixpkgs = sources.nixpkgs.url;
  };
in
# Load an interactive environment:
(import ./. {
  inherit ghc sources pkgs;
}).interactive.overrideAttrs (_: {
  # Export a good NIX_PATH for tools that run in this shell.
  NIX_PATH = lib.concatStringsSep ":"
    (lib.mapAttrsToList (name: value: "${name}=${value}") nix_path);
})
