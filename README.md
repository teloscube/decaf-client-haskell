# DECAF CLI Application and Haskell Client Library

This repository mainly provides a Haskell client library for DECAF APIs and a
small application (1) demonstrating library usage and (2) providing some
utilities for working with DECAF Instances.

> **TODO:** Provide full README.

## Command-line Application

### Installation

If you are using Nix, install from within the codebase:

```sh
nix-env -f default.nix -i
```

... or install directly from GitHub:

```sh
nix-env -f https://github.com/teloscube/decaf-client-haskell/archive/main.tar.gz -i
```

### Shell Completion

Add to your shell init script:

```sh
source <(decafcli --bash-completion-script `which decafcli`)
```

### Usage

```console
$ decafcli --help
decafcli - DECAF Command-line Client Application

Usage: decafcli [--version] COMMAND

  DECAF Client

Available options:
  -h,--help                Show this help text
  --version                Show version

Available commands:
  example-profiles         Produce example yaml file for profiles
  tui                      Runs the TUI application
  serve                    Runs the server application
  microlot                 Run DECAF Microlot query over profiles
  versions                 Get DECAF Barista versions for all profiles
```

### Examples

For the given GraphQL query:

```sh
cat << EOM > /tmp/myquery1.gql
query {
  principals: principal {
    id
    username
  }
}
EOM
```

... dump user ids and usernames:

```sh
decafcli microlot --file-profiles ~/.decaf/profiles.yaml --profile <my-profile> --query /tmp/myquery1.gql
```

For the given GraphQL query:

```sh
cat <<EOM > /tmp/myquery2.gql
query(\$pair: String!) {
  fxrates: ohlc_observation(where: {series: {symbol: {_eq: \$pair}}}) {
    date
    rate: close
  }
}
EOM
```

... dump FX rates of a given FX pair:

```sh
decafcli microlot --file-profiles ~/.decaf/profiles.yaml --profile <my-profile> --query /tmp/myquery2.gql --params '{"pair": "EURUSD"}'
```

## Development

Enter the Nix shell and run all the commands below inside the Nix shell:

```sh
nix-shell
```

Build:

```sh
cabal build -O0
```

Test:

```sh
cabal test -O0
```

Run hlint:

```sh
hlint app/ src/ tests/
```

Format codebase:

```sh
fourmolu -i app/ src/ test/
```

Run:

```sh
cabal run -O0 decafcli -- --help
```

## Release Process

```console
$ nix-shell
[inside-nix-shell]$ ./release.sh -n <VERSION>
```

... where `<VERSION>` follows [Haskell PVP Specification], such as `1.2.3.4`.

Note that there is no `v` prefix.

## License

[BSD 3-Clause License]. Copyright (c) 2020-2023, Teloscube Pte Ltd.

<!-- REFERENCES -->

[BSD 3-Clause License]: ./LICENSE
[Haskell PVP Specification]: https://pvp.haskell.org
