import (builtins.fetchTarball {
  url =
    "https://github.com/NixOS/nixpkgs-channels/archive/029a5de08390bb03c3f44230b064fd1850c6658a.tar.gz";
  sha256 = "03fjkzhrs2avcvdabgm7a65rnyjaqbqdnv4q86qyjkkwg64g5m8x";
}) {
  config = { };
  overlays = [ ];
}
