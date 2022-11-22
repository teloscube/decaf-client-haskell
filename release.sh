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
_appname="decaf-client"

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

_log "Build statically linked executable..."
_outfile_raw="$(./compile-static.sh)"

_log "Copying and renaming statically linked executable (${_outfile_raw})..."
_outfile="decafcli-Linux-x86_64-static"
cp "${_outfile_raw}" "${_outfile}"

_log "Pushing changes to remote..."
git push --follow-tags origin main

_log "Creating the release..."
gh release create "${_version}" --title "v${_version}" --generate-notes

_log "Uploading release artifacts..."
gh release upload "${_version}" "${_outfile}"

_log "Cleaning up release artifacts..."
rm "${_outfile}"

_log "Finished!"
