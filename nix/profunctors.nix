{ mkDerivation, base, base-orphans, bifunctors, comonad
, contravariant, distributive, semigroups, stdenv, tagged
, transformers
}:
mkDerivation {
  pname = "profunctors";
  version = "5.2.2";
  sha256 = "e981e6a33ac99d38a947a749179bbea3c294ecf6bfde41660fe6d8d5a2e43768";
  revision = "3";
  editedCabalFile = "0qf59phdzwa2p3nirq4vnmlxw9qfy1bcskk4nflab1fr4jwnsrs4";
  libraryHaskellDepends = [
    base base-orphans bifunctors comonad contravariant distributive
    semigroups tagged transformers
  ];
  homepage = "http://github.com/ekmett/profunctors/";
  description = "Profunctors";
  license = stdenv.lib.licenses.bsd3;
}
