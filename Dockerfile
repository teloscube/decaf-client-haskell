## Define the base image:
FROM utdemir/ghc-musl:v24-ghc902

## Configure shell:
SHELL ["/bin/bash", "-o", "pipefail", "-c"]

## Install upx:
RUN apk add --no-cache upx

## Add application files:
COPY / /app

## Change the working directory:
WORKDIR /app/

## Rename cabal.project.freeze:
RUN mv cabal.project.freeze.tmpl cabal.project.freeze

## Update cabal:
RUN cabal update

## Build static executable:
RUN cabal build --enable-executable-static && \
    cp "$(cabal list-bin decafcli)" /app/decafcli && \
    strip /app/decafcli && \
    upx /app/decafcli
