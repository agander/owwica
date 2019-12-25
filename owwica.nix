{ mkDerivation, base, bytestring, directory, filepath, regex
, regex-pcre, regex-with-pcre, split, stdenv, template-haskell
, text, HUnit, QuickCheck, base-prelude, cmdargs, hledger-lib
, hspec, interpolate, io-streams, mockery, temporary, zlib
}:
mkDerivation {
  pname = "owwica";
  version = "0.0.20";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring directory filepath regex regex-pcre regex-with-pcre
    split template-haskell text HUnit QuickCheck base-prelude cmdargs
    hledger-lib hspec interpolate io-streams mockery temporary zlib
  ];
  executableHaskellDepends = [
    base directory filepath regex regex-pcre regex-with-pcre split
    template-haskell text HUnit QuickCheck base-prelude cmdargs
    hledger-lib hspec interpolate io-streams mockery temporary zlib
  ];
  homepage = "https://github.com/agander/owwica#readme";
  license = stdenv.lib.licenses.gpl3;
}
