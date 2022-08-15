let
  mkThisPackage = { haskell, name, src, args ? "", overrides ? { } }:
    haskell.callCabal2nixWithOptions name src args overrides;

  mkThisPackageDeps = pkgs: spec:
    let
      thisPackage = mkThisPackage spec;
    in
    pkgs.haskell.lib.compose.getHaskellBuildInputs thisPackage;
in
{
  mkThisPackage = mkThisPackage;
  mkThisPackageDeps = mkThisPackageDeps;
}
