{ nixpkgs ? import <nixpkgs> {} }:

let
  drv = nixpkgs.haskell.packages.ghc96.callCabal2nix "chandlr-server" ./. {};

  env = drv.env.overrideAttrs (oldAttrs: {
    buildInputs = oldAttrs.buildInputs ++ [
      nixpkgs.haskellPackages.cabal-install
    ];
  });

in

  if nixpkgs.lib.inNixShell then env else drv
