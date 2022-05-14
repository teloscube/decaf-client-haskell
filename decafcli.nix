{ mkDerivation, aeson, aeson-combinators, base, base64-bytestring
, blaze-html, blaze-markup, brick, bytestring, case-insensitive
, data-default-class, deriving-aeson, doctest, exceptions, hpack
, hspec, http-conduit, http-types, lib, monad-parallel, network-uri
, optparse-applicative, scotty, string-interpolate, text
, unordered-containers, vector, vty, warp, yaml
}:
mkDerivation {
  pname = "decaf-client";
  version = "0.0.0.4";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base base64-bytestring bytestring case-insensitive
    deriving-aeson exceptions http-conduit http-types network-uri text
    unordered-containers yaml
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson aeson-combinators base base64-bytestring blaze-html
    blaze-markup brick bytestring case-insensitive data-default-class
    deriving-aeson exceptions http-conduit http-types monad-parallel
    network-uri optparse-applicative scotty string-interpolate text
    unordered-containers vector vty warp yaml
  ];
  testHaskellDepends = [
    aeson base base64-bytestring bytestring case-insensitive
    deriving-aeson doctest exceptions hspec http-conduit http-types
    network-uri text unordered-containers yaml
  ];
  prePatch = "hpack";
  homepage = "https://github.com/teloscube/decaf-client-haskell#readme";
  description = "DECAF API Client Suite for Haskell";
  license = lib.licenses.bsd3;
}
