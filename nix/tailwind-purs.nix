{ mkDerivation, base, casing, containers, data-default, directory
, fetchgit, filepath, hpack, hspec, html-parse, lib, MissingH, mtl
, optparse-applicative, parsec, stm, text, transformers, twitch
, with-utf8
}:
mkDerivation {
  pname = "tailwind-purs";
  version = "0.0.2.0";
  src = fetchgit {
    url = "https://github.com/gillchristian/tailwind-purs";
    sha256 = "0i9f78ddvpl7bzsr8pdzsx9qpqqda5fg9sklga1pqbd5i09aa612";
    rev = "ddd12b50623f47cd24e3d9525a9f7433100ef2a6";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base casing containers data-default directory filepath html-parse
    MissingH mtl optparse-applicative parsec stm text transformers
    twitch with-utf8
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base casing containers data-default directory filepath html-parse
    MissingH mtl optparse-applicative parsec stm text transformers
    twitch with-utf8
  ];
  testHaskellDepends = [
    base casing containers data-default directory filepath hspec
    html-parse MissingH mtl optparse-applicative parsec stm text
    transformers twitch with-utf8
  ];
  prePatch = "hpack";
  homepage = "https://github.com/gillchristian/tailwind-purs#readme";
  license = lib.licenses.mit;
}
