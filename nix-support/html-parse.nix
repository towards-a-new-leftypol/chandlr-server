{ nixpkgs ? import <nixpkgs> {} }:

let
  haskellPackages = nixpkgs.haskellPackages;

  src = nixpkgs.fetchFromGitHub {
    owner = "bgamari";
    repo = "html-parse";
    rev = "fcbdfe4ae4da14e9af0e21fabfe2da178f041970";
    sha256 = "sha256-naCRlbDGtcCBDiHOPsZwQ3dzVbFtkC1dr5r6ejdewiI=";
  };

  html-parse = haskellPackages.callCabal2nix "html-parse" src {};
in

  html-parse
