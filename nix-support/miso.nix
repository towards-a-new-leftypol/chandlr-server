{ nixpkgs ? import <nixpkgs> {} }:

let
  haskellPackages = nixpkgs.haskellPackages;

  src = nixpkgs.fetchFromGitHub {
    owner = "dmjio";
    repo = "miso";
    rev = "2e3f27f247025cf397bc413963edea105fa787d4";
    sha256 = "sha256-Sk7rf5rp9T+lkS4WW5saC18kyliqcCkMu7VpbMTPt/I=";
  };

  #src = ../../miso;

  #miso = haskellPackages.callCabal2nix "miso" src {};
  miso = haskellPackages.callCabal2nixWithOptions "miso" src "-fssr" {};
in

  miso
