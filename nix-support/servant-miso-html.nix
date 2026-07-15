{ nixpkgs ? import <nixpkgs> {} }:

let
  haskellPackages = nixpkgs.haskellPackages;

  miso = import ./miso.nix { inherit nixpkgs; };

  src = nixpkgs.fetchFromGitHub {
    owner = "haskell-miso";
    repo = "servant-miso-html";
    rev = "22fd9d18c394f2096ccdeb0558eec6370466c624";
    sha256 = "sha256-vnUrNnlksTzBgqQwxmm0JD5wrsgQwMZ3faz5Duvc8sY=";
  };

  servant-miso-html = haskellPackages.callCabal2nix "servant-miso-html" src {
    miso = miso;
  };
in

  servant-miso-html
