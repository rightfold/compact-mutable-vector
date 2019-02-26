{pkgs ? import ./nix/pkgs.nix {}}:
(import ./. {pkgs = pkgs;}).env.overrideAttrs (p: {
    nativeBuildInputs = p.nativeBuildInputs ++ [
        pkgs.haskellPackages.cabal-install
        pkgs.haskellPackages.ghcid
    ];
})
