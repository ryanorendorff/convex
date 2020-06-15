let

  jupyter-src = builtins.fetchGit {
    url = https://github.com/tweag/jupyterWith;
    rev = "1176b9e8d173f2d2789705ad55c7b53a06155e0f";
  };

  pkgs = import (jupyter-src.outPath + "/nix") {};

  convex = import ../default.nix {inherit pkgs; installOnly = false;};

  haskellPackages = pkgs.haskellPackages.override (old: {
    overrides = pkgs.lib.composeExtensions old.overrides
        (self: hspkgs: {
          lens = hspkgs.callPackage ../nix/lens.nix {};
          profunctors = hspkgs.callPackage ../nix/profunctors.nix {};
          doctest = pkgs.haskell.lib.dontCheck (hspkgs.callPackage ../nix/doctest.nix {});
        });
  });

  jupyter = import jupyter-src {inherit pkgs;};


  iPython = jupyter.kernels.iPythonWith {
    name = "python";
    packages = p: with p; [ numpy matplotlib scipy];
  };

  iHaskell = jupyter.kernels.iHaskellWith {
    name = "haskell";
    haskellPackages = haskellPackages;
    packages = p: with p; [ hvega formatting hmatrix hmatrix-glpk vector random ghc-typelits-natnormalise ghc-typelits-knownnat gnuplot ihaskell-charts Chart cairo convex ];
  };

  jupyterEnvironment =
    jupyter.jupyterlabWith {
      kernels = [ iPython iHaskell ];
      directory = ./jupyterlab;
    };
in
  jupyterEnvironment.env
