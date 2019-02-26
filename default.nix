{pkgs ? import ./nix/pkgs.nix {}}:
pkgs.haskellPackages.callPackage ./compact-mutable-vector.nix {}
