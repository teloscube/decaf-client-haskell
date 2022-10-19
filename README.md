# DECAF API Client Suite For Haskell

A Haskell client library to DECAF API and a small application demonstrating
library usage and providing some utilities for working with DECAF instances.

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
cabal build
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
fourmolu -i app/ src/ test/
```

## Release Process

```console
$ nix-shell
[inside-nix-shell]$ ./release.sh -n <VERSION>
```

... whereby `<VERSION>` follows [Haskell PVP
Specification](https://pvp.haskell.org/), such as `1.2.3.4`. Note that
there is no `v` prefix.

## Static Compilation

We are able to statically build, strip and compress the executable inside a
Docker image. Following command performs the necessary compilation, copy the
file to host machine and inform the user about the path to the copied file:

```sh
./compile-static.sh
```

> **IMPORTANT:** Note that everytime we update Nix sources or add a new
> dependency to Haskell project, we must regenerate the freeze file:
>
> ```sh
> cabal freeze
> mv cabal.project.freeze cabal.project.freeze.tmpl
> ```

## License

[BSD 3-Clause License](./LICENSE). Copyright (c) 2020-2022, Teloscube Pte Ltd.
