{
  description = "A very basic flake";
  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
    let
      compiler = "ghc948";
      overlays = [ haskellNix.overlay
        (final: prev: {
          t-regex =
            final.haskell-nix.project' {
              src = ./.;
              compiler-nix-name = compiler;
              shell.tools = {
                cabal = {};
                hlint = {};
                haskell-language-server = {};
                hasktags = {};
                haskdogs = {};
                ghcid = {};
              };
              shell.buildInputs = with pkgs; [
                just
              ];
            };
        })
      ];
      pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
      flake = pkgs.t-regex.flake {
      };
    in flake // {
      packages.default = flake.packages."t-regex:lib:t-regex";
    });
  nixConfig = {
    # This sets the flake to use the IOG nix cache.
    # Nix should ask for permission before using it,
    # but remove it here if you do not want it to.
    extra-substituters = ["https://cache.zw3rk.com"];
    # extra-substituters = ["https://cache.iog.io"];
    extra-trusted-public-keys = [
      "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
    allow-import-from-derivation = "true";
  };
}
