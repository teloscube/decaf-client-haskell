# DECAF API Client Suite For Haskell

A Haskell client library to DECAF API and an application demonstrating
library usage and providing some utilities.

> **TODO:** Provide full README.

## Command-line Application

### Installation

```sh
stack install
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

## License

[BSD 3-Clause License](./LICENSE). Copyright (c) 2020-2022, Teloscube Pte Ltd.
