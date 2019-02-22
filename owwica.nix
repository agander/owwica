{ mkDerivation, base, bytestring, directory, filepath, regex
, regex-pcre, regex-with-pcre, split, stdenv, template-haskell
, text
}:
mkDerivation {
  pname = "owwica";
  version = "0.0.3";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring directory filepath regex regex-pcre regex-with-pcre
    split template-haskell text
  ];
  executableHaskellDepends = [
    base directory filepath regex regex-pcre regex-with-pcre split
    template-haskell text
  ];
  homepage = "https://github.com/agander/owwica#readme";
  license = stdenv.lib.licenses.gpl3;
}
