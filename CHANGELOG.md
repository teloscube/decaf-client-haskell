# CHANGELOG


<a name="0.0.2"></a>
## [0.0.2](https://github.com/teloscube/decaf-client-haskell/compare/0.0.1...0.0.2) (2023-09-28)

### Chore

* update copyright year
* update .gitignore
* drop static build
* drop old Nix setup in favour of a simpler setup (broken)
* cleanup release artifacts
* upload statically compiled executable to release
* **build:** add static compilation script
* **dev:** reformat {.hlint,package}.yaml file
* **test:** revisit test setup

### Docs

* update README

### Feat

* add multipart/form-data support

### Fix

* adopt /version endpoint response type change in DECAF Estate API

### Refactor

* adopt breaking changes introduced in brick v1.0

### Test

* fix doctests


<a name="0.0.1"></a>
## [0.0.1](https://github.com/teloscube/decaf-client-haskell/compare/0.0.0.5...0.0.1) (2022-10-18)

### Chore

* attend compiler warnings (mainly unusued defs/imports)
* fix release script
* add release script, mention it in README
* adopt fourmolu, drop stylish-haskell, reformat codebase
* remove unnecessary import and dependency
* revisit hlint configuration, remove redundant language pragmas
* remove default extensions in favour of explicit language pragmas
* adopt telos.nix (Nix helpers)
* bump development version to 0.0.0.6
* **deps:** drop Aeson <2 support
* **deps:** revisit dependencies, refine how they are declared
* **dev:** switch from haskell.nix to haskell4nix
* **release:** 0.0.1

### Fix

* capture callstack in DecafClientException Show instance


<a name="0.0.0.5"></a>
## [0.0.0.5](https://github.com/teloscube/decaf-client-haskell/compare/0.0.0.4...0.0.0.5) (2022-06-16)

### Chore

* remove explicit Stack 19.11 YAML file
* purge examples directory, update README for Microlot examples
* revisit language pragmas and language extensions lists
* remove deriving-aeson dependency
* remove haskell4nix Nix module
* add cabal file for v0.0.0.5
* bump development version to 0.0.0.5
* **build:** bump haskell.nix dependency
* **deps:** adopt Stack lts-19.11
* **dev:** sideload development tools from nixpkgs
* **docs:** update README
* **release:** 0.0.0.5
* **test:** enable mock tests


<a name="0.0.0.4"></a>
## [0.0.0.4](https://github.com/teloscube/decaf-client-haskell/compare/0.0.0.3...0.0.0.4) (2022-06-04)

### Chore

* depend on yaml instead of Aeson-yaml. Exit with exitSuccess instead of exitFailure
* add profiles.yaml.tmpl file as a reference
* bump development version to 0.0.0.4
* revisit DecafRemote constructor signature, fix tests
* fix hlint warnings
* add example for Token Authentication
* use string interpolation to generate script body
* make the command work without an option
* fix hlint warnings
* upgrade to lts-18.28
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

* rename ChangeLog.md to CHANGELOG.md
* bump development version to 0.0.0.3
* remove example application from build configuration
* improve Profile module definitions
* update copyright notice and LICENSE text
* **build:** do not use -Werror, use -Wunused-packages, revisit deps
* **deps:** upgrade to lts-18.27
* **dev:** write IDE info when compiling
* **dev:** add Weeder configuration
* **dev:** add HLint configuration, fix lint warnings
* **dev:** add .editorconfig
* **dev:** revisit default language extensions list
* **docs:** fix Haddock warnings
* **release:** 0.0.0.3
* **test:** fix doctest location
* **test:** fix doctests

### Feat

* add Profile data definition and helpers
* add Aeson.{FromJSON,ToJSON} instances to Remote data type
* revisit Credentials data definition

### Refactor

* create Decaf.Client.Internal.Error
* revisit Decaf.Client.Internal.Barista
* put DecafClient and constructor in its own internal module
* add DecafClientException data definition and helpers
* fix Aeson.{FromJSON,ToJSON} instances of Credentials data type
* add value accessor to DecafClientError newtype
* revisit import statements
* add response/request related definitions in respective modules
* change data constructor names for Credentials type
* rename monadic DecafClient constructor, add pure one
* rename Types to Credentials, revisit Decaf.Client exports
* create Decaf.Client.Internal.Apis module
* revisit Decaf.Client.Internal.*
* revisit Decaf.Client.Internal.Pdms
* revisit Decaf.Client.Internal.Microlot
* revisit Decaf.Client.Internal.Http
* revisit Decaf.Client.Internal.Combinators
* move relevant definitions from Types to Remote module
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
* adopt GHC 2021 extension set, revisit imports
* upgrade to lts-18.0
* implement pdms client and add example usage
* add an example for using both Barista and Microlot
* bump development version to 0.0.0.2
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

* refactor the module for DECAF response type
* reformat and apply hlint hints
* define DECAF API response data type, redefine client runners
* init codebase
* allow client builds to use given `Remote`s
* refactor to use 'Remote' DECAF deployment URL definition
* adopt MonadError for request builder, CLI arg parser for Main
* update the Main application
* add DECAF Microlot client, refactor Decaf.Client.Internal
* re-export high-level definitions from top module
* init repository
* refactor Decaf.Client.Internal.{Http,Microlot}
* add DecafClient convenience type and smart constructor
* refactor the library using combinator approach
* reformat code
* simplify Barista client API
* change the dummy Main implementation to a dummier one
* refactor library implementation
* **dev:** add git-chglog configuration
* **docs:** make full Haddock documentation coverage
* **release:** 0.0.0.1
* **test:** enable doctests, fix errors

