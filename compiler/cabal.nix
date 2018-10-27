{ mkDerivation, base, bytestring, data-fix, deepseq
, deriving-compat, ghc, ghc-boot, ghc-paths, hashable, hashmap
, hint, hpack, megaparsec, neat-interpolation, scientific, stdenv
, text, transformers
}:
mkDerivation {
  pname = "architect-compiler";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring data-fix deepseq deriving-compat ghc ghc-boot
    ghc-paths hashable hashmap hint megaparsec neat-interpolation
    scientific text transformers
  ];
  libraryToolDepends = [ hpack ];
  preConfigure = "hpack";
  homepage = "https://github.com/MatrixAI/Haskell-Demo#readme";
  license = stdenv.lib.licenses.asl20;
}
