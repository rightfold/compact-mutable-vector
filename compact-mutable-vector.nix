{mkDerivation, base, compact, ghc-prim, hspec, primitive, vector}:
mkDerivation {
    pname = "compact-mutable-vector";
    version = "0.0.0.1";
    license = null;
    src = builtins.filterSource (p: t: p != toString ./dist &&
                                       p != toString ./result)
                                ./.;
    buildDepends = [
        base
        compact
        ghc-prim
        hspec
        primitive
        vector
    ];
}
