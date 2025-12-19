{ nixpkgs ? import <nixpkgs> {} }:

let
  haskellPackages = nixpkgs.haskellPackages;

  src = nixpkgs.fetchFromGitHub {
    owner = "dmjio";
    repo = "miso";
    rev = "3bb245c5eef10d040de1d05bae22e6898381e4ed";
    sha256 = "sha256-1Uf/VbVO9ifo4DQP7ZpFQZBYAD0LndzvnwyV7xqvuzc=";
  };

  # src = ../../miso;

  #miso = haskellPackages.callCabal2nix "miso" src {};
  miso = haskellPackages.callCabal2nixWithOptions "miso" src "-fssr" {};
in

  miso
