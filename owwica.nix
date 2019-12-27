{ mkDerivation, base, base-prelude, bytestring, cmdargs
, directory, filepath, hledger-lib, hpack, hspec, hspec-discover
, HUnit, interpolate, io-streams, mockery, QuickCheck, regex-pcre
, split, stdenv, temporary, text, zlib
}:
mkDerivation {
  pname = "owwica";
  version = "0.0.20";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base base-prelude bytestring directory filepath hledger-lib hspec
    io-streams QuickCheck regex-pcre split text zlib
  ];
  libraryToolDepends = [ hpack hspec-discover ];
  executableHaskellDepends = [
    base base-prelude bytestring cmdargs directory filepath hledger-lib
    hspec io-streams QuickCheck regex-pcre split text zlib
  ];
  executableToolDepends = [ hspec-discover ];
  testHaskellDepends = [
    base base-prelude bytestring directory filepath hledger-lib hspec
    HUnit interpolate io-streams mockery QuickCheck regex-pcre split
    temporary text zlib
  ];
  testToolDepends = [ hspec-discover ];
  prePatch = "hpack";
  homepage = "https://gitlab.com/agander/owwica/blob/master/README.md";
  license = stdenv.lib.licenses.gpl3;
}

