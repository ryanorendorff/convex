{ pkgs ? import ./nix/nixpkgs.nix, installOnly ? true }:

let

  inherit (pkgs) lib;

  name = "dependent-convex-optimization";

  regexes = [ ".*.cabal$" "^src.*" "^main.*" "^Setup.hs$" "LICENSE" ];

  src = builtins.path {
    path = ./.;
    name = "${name}-src";
    filter = path: type:
      let relPath = lib.removePrefix (toString ./. + "/") (toString path);
      in lib.any (re: builtins.match re relPath != null) regexes;
  };

  dependent-convex = pkgs.haskellPackages.callCabal2nix name src { };

in

  if installOnly then
    pkgs.haskell.lib.justStaticExecutables dependent-convex
  else
    dependent-convex
