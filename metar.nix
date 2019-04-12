{ mkDerivation, base, bytestring, checkers, deriving-compat, HTTP
, http-client, lens, network-uri, QuickCheck, semigroupoids
, semigroups, stdenv, tagsoup, tagsoup-selection, tasty
, tasty-hunit, tasty-quickcheck, transformers, wreq
}:
mkDerivation {
  pname = "metar";
  version = "0.0.3";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring deriving-compat HTTP http-client lens network-uri
    semigroupoids semigroups tagsoup tagsoup-selection transformers
    wreq
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base checkers lens QuickCheck tasty tasty-hunit tasty-quickcheck
  ];
  homepage = "https://github.com/tonymorris/metar";
  description = "Australian METAR";
  license = stdenv.lib.licenses.bsd3;
}
