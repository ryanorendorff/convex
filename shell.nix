{ pkgs ? import ./nix/nixpkgs.nix }:

let

  inherit pkgs;

  dependent-pkgs = import ./default.nix {inherit pkgs; installOnly = false;};

  # Generate the hoogle database for all packages listed in the cabal file.
  hoogle-path = pkgs.path + "/pkgs/development/haskell-modules/hoogle.nix";
  hoogle-with-package-database = pkgs.callPackage hoogle-path {
    hoogle = pkgs.haskellPackages.hoogle;
    packages = dependent-pkgs.getBuildInputs.haskellBuildInputs;
  };

  # Mac has some problematic packages.
  iflinux = p: if builtins.currentSystem == "x86_64-linux" then p else [];
  linux-only-haskell-tools = iflinux [pkgs.haskellPackages.apply-refact];

  tex = pkgs.texlive.combine {
    inherit (pkgs.texlive)
    scheme-medium

    appendixnumberbeamer
    beamer
    beamertheme-metropolis
    biblatex
    cm-super
    cmll
    ec
    footmisc
    framed
    fvextra
    ifplatform
    lazylist
    lm-math
    logreq
    mathpartir
    minted
    pgfopts
    polytable
    stmaryrd
    upquote
    wasysym
    xstring
    ;
  };

  haskell-developer-tools = with pkgs.haskellPackages;
  [
    cabal-install

    # Editor tools (for use with the Spacemacs Haskell layer)
    haskintex
    hasktags
    hlint
    stylish-haskell
    hoogle-with-package-database

    # Stuff to compile the presentation with.
    lhs2tex
    tex
    pkgs.python37Packages.pygments

    # We need this for the GNU plot mentioned in the default.nix. Ideally we
    # should actually package this up in the original environment.... Use
    # haskellPackages.developPackage to solve this problem?
    pkgs.gnuplot
  ] ++ linux-only-haskell-tools;


in

  dependent-pkgs.env.overrideAttrs (
    attrs: { buildInputs = attrs.buildInputs ++ haskell-developer-tools; }
  )
