{ mkDerivation, base, checkers, deriving-compat, HTTP, lens
, network-uri, QuickCheck, semigroupoids, semigroups, stdenv
, tagsoup, tagsoup-selection, tasty, tasty-hunit, tasty-quickcheck
, transformers
}:
mkDerivation {
  pname = "metar";
  version = "0.0.2";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base deriving-compat HTTP lens network-uri semigroupoids semigroups
    tagsoup tagsoup-selection transformers
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base checkers lens QuickCheck tasty tasty-hunit tasty-quickcheck
  ];
  homepage = "https://github.com/tonymorris/metar";
  description = "Australian METAR";
  license = stdenv.lib.licenses.bsd3;
}
