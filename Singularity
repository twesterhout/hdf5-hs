Bootstrap: docker
From: utdemir/ghc-musl:v16-ghc884
Stage: build

%post -c /bin/bash
    set -e

    curl -sSL https://get.haskellstack.org/ | sh

    apk update
    apk add hdf5-dev
    stack --version
