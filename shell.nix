{ compiler ? "ghc902"
, ...
}:

let
  ## Import sources:
  sources = import ./nix/sources.nix;

  ## Import utilities:
  utils = import ./nix/utils.nix;

  ## Pinned nixpkgs:
  pkgs = import sources.nixpkgs { };

  ## Get the haskell set:
  haskell = pkgs.haskell.packages.${compiler};

  ## Get this package:
  thisPackageDeps = utils.mkThisPackageDeps pkgs {
    haskell = haskell;
    name = "decaf-client";
    src = ./.;
    args = "--no-haddock";
  };

  ## Get our GHC for development:
  ghc = haskell.ghcWithPackages (_: thisPackageDeps);
in
pkgs.mkShell {
  buildInputs = [
    ## Fancy stuff:
    pkgs.figlet
    pkgs.lolcat

    ## Release stuff:
    pkgs.busybox
    pkgs.gh
    pkgs.git
    pkgs.git-chglog

    ## Haskell stuff:
    ghc
    pkgs.cabal-install
    pkgs.cabal2nix
    pkgs.haskell-language-server
    pkgs.haskellPackages.apply-refact
    pkgs.hlint
    pkgs.stylish-haskell
  ];

  shellHook = ''
    figlet -w 999 "DECAF CLIENT DEV SHELL" | lolcat -S 42
  '';
}
