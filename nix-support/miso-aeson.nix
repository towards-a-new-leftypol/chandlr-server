{ nixpkgs ? import <nixpkgs> {} }:

let
  haskellPackages = nixpkgs.haskellPackages;

  miso = import ./miso.nix { inherit nixpkgs; };

  src = nixpkgs.fetchFromGitHub {
    owner = "haskell-miso";
    repo = "miso-aeson";
    rev = "7a71eb7ee92229216bf12ca88885bdea34a4caa2";
    sha256 = "sha256-vEnchpSKQkqNOKYpj8DRwAmYbVU8/zB2IuzOh3pYA6Y=";
  };

  drv = haskellPackages.callCabal2nix "miso-aeson" src {
    miso = miso;
  };
in

  drv

