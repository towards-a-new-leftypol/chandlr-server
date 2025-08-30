{ nixpkgs ? import <nixpkgs> {} }:

let
  http-conduit = import ./app/Common/nix-support/http-conduit.nix { inherit nixpkgs; };
  html-parse = import ./nix-support/html-parse.nix { inherit nixpkgs; };
  servant-miso-html = import ./nix-support/servant-miso-html.nix { inherit nixpkgs; };
  miso = import ./nix-support/miso.nix { inherit nixpkgs; };

  drv = nixpkgs.haskellPackages.callCabal2nix "chandlr-server" ./. {
    http-conduit = http-conduit.http-conduit;
    html-parse = html-parse;
    miso = miso;
    servant-miso-html = servant-miso-html;
  };

  env = drv.env.overrideAttrs (oldAttrs: {
    buildInputs = oldAttrs.buildInputs ++ [
      nixpkgs.haskellPackages.cabal-install
    ];
  });

in

  if nixpkgs.lib.inNixShell then env else drv
