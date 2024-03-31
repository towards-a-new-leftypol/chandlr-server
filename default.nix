{ nixpkgs ? import <nixpkgs> {} }:

let
  http-conduit = import ./app/Common/nix-support/http-conduit.nix { inherit nixpkgs; };
  html-parse = import ./nix-support/html-parse.nix { inherit nixpkgs; };

  drv = nixpkgs.haskellPackages.callCabal2nix "chandlr-server" ./. {
    http-conduit = http-conduit.http-conduit;
    html-parse = html-parse;
  };

  env = drv.env.overrideAttrs (oldAttrs: {
    buildInputs = oldAttrs.buildInputs ++ [
      nixpkgs.haskellPackages.cabal-install
    ];
  });

in

  if nixpkgs.lib.inNixShell then env else drv
