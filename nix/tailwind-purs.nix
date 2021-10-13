{ mkDerivation, base, casing, containers, data-default, directory
, fetchgit, filepath, hpack, hspec, html-parse, lib, MissingH, mtl
, optparse-applicative, parsec, stm, text, transformers, twitch
}:
mkDerivation {
  pname = "tailwind-purs";
  version = "0.0.1.0";
  src = fetchgit {
    url = "git://github.com/gillchristian/tailwind-purs";
    sha256 = "04cbqp1kbiavp9vbrcbg0kn42hpsdwnlvja7ga34dg7zz97mi3sd";
    rev = "87474189c951320959797b7a77488343de203771";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base casing containers data-default directory filepath html-parse
    MissingH mtl optparse-applicative parsec stm text transformers
    twitch
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base casing containers data-default directory filepath html-parse
    MissingH mtl optparse-applicative parsec stm text transformers
    twitch
  ];
  testHaskellDepends = [
    base casing containers data-default directory filepath hspec
    html-parse MissingH mtl optparse-applicative parsec stm text
    transformers twitch
  ];
  prePatch = "hpack";
  homepage = "https://github.com/gillchristian/tailwind-purs#readme";
  license = lib.licenses.mit;
}
