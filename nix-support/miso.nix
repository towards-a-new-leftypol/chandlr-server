{ nixpkgs ? import <nixpkgs> {} }:

let
  haskellPackages = nixpkgs.haskellPackages;

  src = nixpkgs.fetchFromGitHub {
    owner = "dmjio";
    repo = "miso";
    rev = "8b2b2fb376c641f3c15b438815ec54f25d3532fc";
    sha256 = "sha256-kXe17PZXLtbmTPerjiZmHDRdJrvtkDM6/uCEIezU4vQ=";
  };

  # src = ../../miso;

  #miso = haskellPackages.callCabal2nix "miso" src {};
  miso = haskellPackages.callCabal2nixWithOptions "miso" src "-fssr" {};
in

  miso
