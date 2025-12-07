{ nixpkgs ? import <nixpkgs> {} }:

let
  haskellPackages = nixpkgs.haskellPackages;

  src = nixpkgs.fetchFromGitHub {
    owner = "dmjio";
    repo = "miso";
    rev = "65db9a6b3cc3391157787639abc9b6bdf45c80f8";
    sha256 = "sha256-C90aSeyOP565+HguZtf2tZ+tUXE23xWghcWTB1+I+IA=";
  };

  #src = ../../miso;

  #miso = haskellPackages.callCabal2nix "miso" src {};
  miso = haskellPackages.callCabal2nixWithOptions "miso" src "-fssr" {};
in

  miso
