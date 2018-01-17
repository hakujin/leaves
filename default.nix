{ pkgs }:

pkgs.haskellPackages.callCabal2nix "leaves" ./. {}
