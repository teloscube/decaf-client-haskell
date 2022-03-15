# DECAF API Client Suite For Haskell

> **TODO:** Provide full README.

## Command-line Application

### Usage

```
$ decafcli microlot --help
Usage: decafcli microlot --config CONFIG --profile PROFILE --query QUERY
                         [--param PARAM]
  Run Barista

Available options:
  --config CONFIG          Path to generic DECAF configuration file
  --profile PROFILE        Profile name
  --query QUERY            Microlot GraphQL query
  --param PARAM            Microlot GraphQL query parameters
  -h,--help                Show this help text
```

### Examples

Dump user ids and usernames:

```
decafcli microlot --config ~/.decaf.json --profile telosinvest --query examples/microlot/queries/principals.gql
```

Dump FX rates for a given FX pair:

```
decafcli microlot --config ~/.decaf.json --profile telosinvest --query examples/microlot/queries/fxrates.gql --params '{"pair": "EURUSD"}'
```

## License

[BSD 3-Clause License](./LICENSE). Copyright (c) 2020-2022, Teloscube Pte Ltd.
