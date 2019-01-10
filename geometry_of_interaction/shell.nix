with import <nixpkgs> {};
let ghc = haskellPackages.ghcWithPackages (p: with p; [ bifunctors mtl exceptions ]);
in
pkgs.stdenv.mkDerivation {
  name = "haskell-env";
  buildInputs = [ ghc ];
  shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
}
