{ compiler ? "ghc902"
, ...
}:

let
  ## Import sources:
  sources = import ./nix/sources.nix;

  ## Import utilities:
  utils = import ./nix/utils.nix;

  ## Import nixpkgs:
  pkgs = import sources.nixpkgs { };

  ## Get the haskell set:
  haskell = pkgs.haskell.packages.${compiler};

  ## Get this package:
  thisPackage = utils.mkThisPackage {
    haskell = haskell;
    name = "decaf-client";
    src = ./.;
    args = "--no-check --no-haddock";
  };
in
pkgs.haskell.lib.justStaticExecutables thisPackage
