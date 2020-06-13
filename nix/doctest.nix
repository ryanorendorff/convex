{ mkDerivation, base, base-compat, code-page, deepseq, directory
, filepath, ghc, ghc-paths, hspec, HUnit, mockery, process
, QuickCheck, setenv, silently, stdenv, stringbuilder, syb
, transformers, with-location
}:
mkDerivation {
  pname = "doctest";
  version = "0.15.1";
  sha256 = "ed32ae5bfc24d0ef2594dc775a3bd3caf45ec381dbe97145ba3bf147e7a4c723";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base base-compat code-page deepseq directory filepath ghc ghc-paths
    process syb transformers
  ];
  executableHaskellDepends = [
    base base-compat code-page deepseq directory filepath ghc ghc-paths
    process syb transformers
  ];
  testHaskellDepends = [
    base base-compat code-page deepseq directory filepath ghc ghc-paths
    hspec HUnit mockery process QuickCheck setenv silently
    stringbuilder syb transformers with-location
  ];
  homepage = "https://github.com/sol/doctest#readme";
  description = "Test interactive Haskell examples";
  license = stdenv.lib.licenses.mit;
}
