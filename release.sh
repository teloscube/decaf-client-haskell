#!/usr/bin/env bash

## Stop on errors:
set -eo pipefail

## Helper function to print log messages.
_log() {
  echo "[RELEASE LOG]" "${@}"
}

## Helper function to print errors.
_error() {
  echo 1>&2 "[RELEASE ERROR]" "${@}"
}

## Helper function to print usage.
_usage() {
  echo "Usage: $0 [-h] -n <VERSION>"
}

## Declare variables:
_version=""
_regname="registry.docker.decafhub.com"
_appname="decaf-function-data-export"

## Parse command line arguments:
while getopts "n:h" o; do
  case "${o}" in
  n)
    _version="${OPTARG}"
    ;;
  h)
    _usage
    exit 0
    ;;
  *)
    _usage 1>&2
    exit 1
    ;;
  esac
done
shift $((OPTIND - 1))

_log "Checking variables..."
if [ -z "${_version}" ]; then
  _usage 1>&2
  exit 1
else
  _log "Version is \"${_version}\". Proceeding..."
  _log "Registry name is \"${_regname}\". Proceeding..."
  _log "Application name is \"${_appname}\". Proceeding..."
fi

_log "Checking git repository state..."
if [[ -z "$(git status --porcelain)" ]]; then
  _log "Git repository is clean. Proceeding..."
else
  _error "Git repository is not clean. Aborting..."
  exit 1
fi

_log "Updating application version..."
sed -i -E "s/^version:([ ]+).*/version:\\1${_version}/g" package.yaml
hpack

_log "Generating changelog..."
git-chglog --output CHANGELOG.md --next-tag "${_version}"

_log "Staging changes..."
git add package.yaml "${_appname}.cabal" CHANGELOG.md

_log "Committing changes..."
git commit -m "chore(release): ${_version}"

_log "Tagging version..."
git tag -a -m "Release ${_version}" "${_version}"

_log "Building..."
docker load <$(nix-build docker.nix --arg registry "\"${_regname}\"" --arg repository "\"${_appname}\"" --arg tag "\"${_version}\"")

_log "Pushing build..."
docker push "${_regname}/${_appname}:${_version}"

_log "Pushing changes to remote..."
git push --follow-tags origin main

_log "Creating the release..."
gh release create "${_version}" --title "${_version}" --generate-notes

_log "Finished!"
