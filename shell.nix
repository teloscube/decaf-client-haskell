{ packages ? import ./default.nix
}:
let
  inherit (packages) pkgs;
  extraTools = [
    pkgs.hlint
    pkgs.stylish-haskell  ## Note: I couldn't get this work under `tools` section due to Cabal version mismatch.
  ];
in

packages.shellFor {
  tools = {
    cabal = "3.4.0.0";
    haskell-language-server = "latest";
  };

  nativeBuildInputs = extraTools;
}
