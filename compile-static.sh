#!/usr/bin/env bash

## Build the Docker image statically:
docker build -t decafcli . >&2

## Create a Docker container from the Docker image built above and get its identifier:
_id="$(docker create decafcli)"

## Create a temporary directory and define the output path:
_path="$(mktemp -d)/decafcli"

## Copy the statically compiled executable:
docker cp "${_id}:/app/decafcli" "${_path}"

## Remove the container:
docker rm -v "${_id}" >&2

## Inform the enduser about the path:
echo "${_path}"
