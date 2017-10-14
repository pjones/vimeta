# These arguments are so you can override settings from the command
# line using the `nix-hs' tool.
{ nixpkgs   ? import <nixpkgs> { }
, compiler  ? "default"
, profiling ? false
}:

let
  pkgs = nixpkgs;

  buildInputs = with pkgs; [
    # List extra dependencies here.
    atomicparsley
  ];

in
  pkgs.nix-hs.interactive ./vimeta.nix
    { inherit compiler profiling buildInputs; }
