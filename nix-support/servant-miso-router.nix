{ nixpkgs ? import <nixpkgs> {} }:

let
  haskellPackages = nixpkgs.haskellPackages;

  miso = import ./miso.nix { inherit nixpkgs; };
  servant-miso-html = import ./servant-miso-html.nix { inherit nixpkgs; };

  src = nixpkgs.fetchFromGitHub {
    owner = "haskell-miso";
    repo = "servant-miso-router";
    rev = "c8a3994f14dae4e5c35df0896e0ac961200c1108";
    sha256 = "sha256-DjfCXTRBl0LDuMVIk75U6BIy/tXwP59xV1NcUhJlizI=";
  };

  drv = haskellPackages.callCabal2nix "servant-miso-router" src {
    miso = miso;
    servant-miso-html = servant-miso-html;
  };
in

  drv

