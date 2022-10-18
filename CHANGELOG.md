# CHANGELOG


<a name="0.0.1"></a>
## [0.0.1](https://github.com/teloscube/decaf-client-haskell/compare/0.0.0.5...0.0.1) (2022-10-18)

### Chore

* fix release script
* add release script, mention it in README
* adopt fourmolu, drop stylish-haskell, reformat codebase
* remove unnecessary import and dependency
* attend compiler warnings (mainly unusued defs/imports)
* revisit hlint configuration, remove redundant language pragmas
* remove default extensions in favour of explicit language pragmas
* adopt telos.nix (Nix helpers)
* bump development version to 0.0.0.6
* **deps:** drop Aeson <2 support
* **deps:** revisit dependencies, refine how they are declared
* **dev:** switch from haskell.nix to haskell4nix

### Fix

* capture callstack in DecafClientException Show instance


<a name="0.0.0.5"></a>
## [0.0.0.5](https://github.com/teloscube/decaf-client-haskell/compare/0.0.0.4...0.0.0.5) (2022-06-16)

### Chore

* bump development version to 0.0.0.5
* purge examples directory, update README for Microlot examples
* add cabal file for v0.0.0.5
* revisit language pragmas and language extensions lists
* remove deriving-aeson dependency
* remove haskell4nix Nix module
* remove explicit Stack 19.11 YAML file
* **build:** bump haskell.nix dependency
* **deps:** adopt Stack lts-19.11
* **dev:** sideload development tools from nixpkgs
* **docs:** update README
* **release:** 0.0.0.5
* **test:** enable mock tests


<a name="0.0.0.4"></a>
## [0.0.0.4](https://github.com/teloscube/decaf-client-haskell/compare/0.0.0.3...0.0.0.4) (2022-06-04)

### Chore

* bump development version to 0.0.0.4
* depend on yaml instead of Aeson-yaml. Exit with exitSuccess instead of exitFailure
* add example for Token Authentication
* make the command work without an option
* upgrade to lts-18.28
* fix hlint warnings
* use string interpolation to generate script body
* fix hlint warnings
* revisit DecafRemote constructor signature, fix tests
* add profiles.yaml.tmpl file as a reference
* **build:** nixify
* **cli:** add header, basic and key authorization headers
* **cli:** separate graphq url from graphiql. Add console url.
* **dev:** adopt haskell.nix
* **docs:** update README
* **release:** 0.0.0.4
* **test:** start mock tests using httpbin

### Feat

* add graphiql for microlot and module-pdms using key authentication
* implement profile details page
* add Web application that lists profiles on homepage
* implement example-profiles sub-command
* start TUI application
* finish major refactoring for new API (with MonadThrow)
* add request combinator indicating checking of response codes

### Fix

* **cli:** fix module-pdms graphiql url

### Refactor

* revisit main app module layout
* reformat table, finish TUI application
* start major refactoring for new API (with MonadThrow)
* rename exception module
* revisit DecafCredentials module
* revisit DecafProfile module
* revisit DecafRemote module
* revisit DecafRequest and DecafResponse modules


<a name="0.0.0.3"></a>
## [0.0.0.3](https://github.com/teloscube/decaf-client-haskell/compare/0.0.0.2...0.0.0.3) (2022-03-16)

### Chore

* improve Profile module definitions
* bump development version to 0.0.0.3
* update copyright notice and LICENSE text
* remove example application from build configuration
* rename ChangeLog.md to CHANGELOG.md
* **build:** do not use -Werror, use -Wunused-packages, revisit deps
* **deps:** upgrade to lts-18.27
* **dev:** revisit default language extensions list
* **dev:** add Weeder configuration
* **dev:** add HLint configuration, fix lint warnings
* **dev:** add .editorconfig
* **dev:** write IDE info when compiling
* **docs:** fix Haddock warnings
* **release:** 0.0.0.3
* **test:** fix doctests
* **test:** fix doctest location

### Feat

* add Profile data definition and helpers
* add Aeson.{FromJSON,ToJSON} instances to Remote data type
* revisit Credentials data definition

### Refactor

* change data constructor names for Credentials type
* rename monadic DecafClient constructor, add pure one
* put DecafClient and constructor in its own internal module
* add DecafClientException data definition and helpers
* fix Aeson.{FromJSON,ToJSON} instances of Credentials data type
* add value accessor to DecafClientError newtype
* revisit import statements
* rename Types to Credentials, revisit Decaf.Client exports
* add response/request related definitions in respective modules
* move relevant definitions from Types to Remote module
* create Decaf.Client.Internal.Error
* create Decaf.Client.Internal.Apis module
* revisit Decaf.Client.Internal.*
* revisit Decaf.Client.Internal.Pdms
* revisit Decaf.Client.Internal.Microlot
* revisit Decaf.Client.Internal.Http
* revisit Decaf.Client.Internal.Combinators
* revisit Decaf.Client.Internal.Barista
* remove redundant Utils definitions
* **main:** refactor decafcli application for batch Microlot query runner

### BREAKING CHANGE


mkDecafClient function that consumes a Data.Text.Text
URL and works in MonadError context is now renamed to
mkDecafClientE. We added a new mkDecafClient function that consumes a
DECAF Instance Remote and is pure.

Data definition for Credentials has
changed. Call-sites must adopt new data definition.

1. 'CredentialsBasic' now consumes a value of type 'BasicCredentials'
2. 'CredentialsKey' now consumes a value of type 'KeyCredentials'
3. 'Data.Aeson.FromJSON' and 'Data.Aeson.ToJSON' instances are added to 'Credentials'.


<a name="0.0.0.2"></a>
## [0.0.0.2](https://github.com/teloscube/decaf-client-haskell/compare/0.0.0.1...0.0.0.2) (2021-06-28)

### Chore

* update CHANGELOG template
* replace `DecafClientM` with `MonadError DecafClientError`
* bump development version to 0.0.0.2
* adopt GHC 2021 extension set, revisit imports
* upgrade to lts-18.0
* implement pdms client and add example usage
* add an example for using both Barista and Microlot
* **release:** 0.0.0.2
* **style:** apply reformatting, update doc comments
* **test:** fix tests, move doctest.hs under test directory

### Feat

* expose remote definition on DecafClient
* **cli:** add CLI command for running Microlot queries

### Fix

* **app:** adopt added PDMS client in the main app


<a name="0.0.0.1"></a>
## 0.0.0.1 (2020-09-27)

### Chore

* init repository
* init codebase
* reformat and apply hlint hints
* add DECAF Microlot client, refactor Decaf.Client.Internal
* allow client builds to use given `Remote`s
* refactor to use 'Remote' DECAF deployment URL definition
* adopt MonadError for request builder, CLI arg parser for Main
* update the Main application
* add DecafClient convenience type and smart constructor
* re-export high-level definitions from top module
* refactor the module for DECAF response type
* define DECAF API response data type, redefine client runners
* refactor Decaf.Client.Internal.{Http,Microlot}
* refactor the library using combinator approach
* reformat code
* simplify Barista client API
* change the dummy Main implementation to a dummier one
* refactor library implementation
* **dev:** add git-chglog configuration
* **docs:** make full Haddock documentation coverage
* **release:** 0.0.0.1
* **test:** enable doctests, fix errors

