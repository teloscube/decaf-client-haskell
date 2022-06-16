{ packages ? import ./default.nix
}:

let
  inherit (packages) pkgs;

  ## TODO: Remove sideloading development tools once we get them
  ## installed via shellFor.tools. See related NOTE below.
  sideload = import (fetchTarball https://github.com/NixOS/nixpkgs/archive/22.05.tar.gz) { };
  extraTools = [
    sideload.hlint
    sideload.stylish-haskell
    sideload.haskell-language-server
  ];
in
packages.shellFor {
  ## NOTE: Failed to activate shellFor.tools because of some
  ## (hopefully temporary) issues with haskell.nix and our lack of
  ## knowledge how to address them. Therefore, we are importing
  ## development tools from nixpkgs v22.05. See above.
  #
  # tools = {
  #   cabal = "latest";
  #   haskell-language-server = {
  #     ## TODO: Observe https://github.com/input-output-hk/haskell.nix/issues/1512
  #     modules = [{
  #       reinstallableLibGhc = false;
  #       nonReinstallablePkgs = [
  #         "rts"
  #         "ghc-heap"
  #         "ghc-prim"
  #         "integer-gmp"
  #         "integer-simple"
  #         "base"
  #         "deepseq"
  #         "array"
  #         "ghc-boot-th"
  #         "pretty"
  #         "template-haskell"
  #         "ghcjs-prim"
  #         "ghcjs-th"
  #         "ghc-bignum"
  #         "exceptions"
  #         "stm"
  #         "ghc-boot"
  #         "ghc"
  #         "Cabal"
  #         "Win32"
  #         "array"
  #         "binary"
  #         "bytestring"
  #         "containers"
  #         "directory"
  #         "filepath"
  #         "ghc-boot"
  #         "ghc-compact"
  #         "ghc-prim"
  #         "hpc"
  #         "mtl"
  #         "parsec"
  #         "process"
  #         "text"
  #         "time"
  #         "transformers"
  #         "unix"
  #         "xhtml"
  #         "terminfo"
  #       ];
  #     }];
  #   };
  # };

  nativeBuildInputs = extraTools;
}
