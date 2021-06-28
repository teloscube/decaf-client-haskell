# CHANGELOG


<a name="0.0.0.2"></a>
## [0.0.0.2](https://github.com/teloscube/decaf-client-haskell/compare/0.0.0.1...0.0.0.2) (2021-06-28)

### Chore

* replace `DecafClientM` with `MonadError DecafClientError`
* bump development version to 0.0.0.2
* adopt GHC 2021 extension set, revisit imports
* upgrade to lts-18.0
* implement pdms client and add example usage
* add an example for using both Barista and Microlot
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

