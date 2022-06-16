# DECAF API Client Suite For Haskell

A Haskell client library to DECAF API and a small application demonstrating
library usage and providing some utilities for working with DECAF instances.

> **TODO:** Provide full README.

## Command-line Application

### Installation

If you are not on Nix and have installed Stack:

```sh
stack install
```

If you are using Nix, build:

```sh
nix-build -A decaf-client.components.exes.decafcli
```

... or install from within the codebase:

```sh
nix-env -f default.nix -iA decaf-client.components.exes.decafcli
```

... or install directly from GitHub:

```sh
nix-env -f https://github.com/teloscube/decaf-client-haskell/archive/main.tar.gz -iA decaf-client.components.exes.decafcli
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

Dump user ids and usernames:

```sh
decafcli microlot --file-profiles ~/.decaf/profiles.yaml --profile <my-profile> --query examples/microlot/queries/principals.gql
```

Dump FX rates for a given FX pair:

```sh
decafcli microlot --file-profiles ~/.decaf/profiles.yaml --profile <my-profile> --query examples/microlot/queries/fxrates.gql --params '{"pair": "EURUSD"}'
```

## Development

Enter the Nix shell and run all the commands below inside the Nix shell:

```sh
nix-shell
```

Build:

```sh
stack build
```

Open VSCode if you wish to do so:

```sh
code .
```

Run hlint:

```sh
hlint app/ src/ test/
```

Format codebase:

```sh
stylish-haskell -ir app/ src/ test/
```

## License

[BSD 3-Clause License](./LICENSE). Copyright (c) 2020-2022, Teloscube Pte Ltd.
