{ nixpkgs ? import <nixpkgs> {} }:

let
  haskellPackages = nixpkgs.haskellPackages;

  miso = import ./miso.nix { inherit nixpkgs; };
  servant-miso-html = import ./servant-miso-html.nix { inherit nixpkgs; };

  src = nixpkgs.fetchFromGitHub {
    owner = "haskell-miso";
    repo = "servant-miso-router";
    rev = "0c828e0ba30ee7a446ce8999288b32b7f6425dd1";
    sha256 = "sha256-2Vkheb2iNDFWNAToO+r8rMY3OAA6LlUtgxiCWRm0wAY=";
  };

  drv = haskellPackages.callCabal2nix "servant-miso-router" src {
    miso = miso;
    servant-miso-html = servant-miso-html;
  };
in

  drv

