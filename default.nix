let
  ## Read in the Niv sources:
  sources = import ./nix/sources.nix { };

  ## Fetch the haskell.nix commit we have pinned with Niv:
  haskellNix = import sources.haskellNix { };

  ## Import nixpkgs and pass the haskell.nix provided nixpkgsArgs
  pkgs = import haskellNix.sources.nixpkgs-unstable haskellNix.nixpkgsArgs;
in
pkgs.haskell-nix.project {
  ## Note that 'cleanGit' cleans a source directory based on the files known by git.
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "haskell-nix-project";
    src = ./.;
  };
}
