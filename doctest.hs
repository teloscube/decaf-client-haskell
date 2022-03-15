import Test.DocTest (doctest)


main :: IO ()
main = doctest
  [ "-XConstraintKinds"
  , "-XDeriveGeneric"
  , "-XExistentialQuantification"
  , "-XFlexibleContexts"
  , "-XGeneralizedNewtypeDeriving"
  , "-XOverloadedStrings"
  , "-XRankNTypes"
  , "-XScopedTypeVariables"
  , "-XTupleSections"
  , "-isrc", "src"
  ]
