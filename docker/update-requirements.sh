#!/bin/bash -ex

LEASTAUTHORITY=${PWD}/$(dirname $(dirname $0))

NIX_STORE=/nix
CACHE_VOLUME=nix-cache-implicit-init

build_command="
cd /tmp
nix-env --install --attr nixpkgs.glibc nixpkgs.glibcLocales nixpkgs.gitMinimal
git clone --depth=1 https://github.com/garbas/pypi2nix
cd pypi2nix
nix-env -f release.nix -iA build.\"x86_64-linux\"
cd /leastauthority.com
mkdir -p /nix/pypi2nix/cache
pypi2nix \
    --python-version 2.7 \
\
    `# it would be nice to run test suites with --enable-tests but they all fail` \
    --cache-dir /nix/pypi2nix/cache \
\
    `# anything that uses cffi - eg cryptography` \
    --extra-build-inputs libffi \
\
    `# cryptography` \
    --extra-build-inputs openssl \
\
    `# lxml` \
    --extra-build-inputs libxml2 \
    --extra-build-inputs libxslt \
\
    `# upstream unittest2 cannot be built into a wheel but our forked version can be` \
    --extra-env PIP_FIND_LINKS=${LEASTAUTHORITY}/wheelhouse \
\
    --requirements requirements.txt
"

docker run \
    --rm \
    --interactive \
    --volume ${CACHE_VOLUME}:${NIX_STORE} \
    --volume ${LEASTAUTHORITY}:/leastauthority.com \
    -e 'LC_ALL=en_US.UTF-8' \
    -e 'LANG=en_US.UTF-8' \
    -e 'LOCALE_ARCHIVE=/root/.nix-profile/lib/locale/locale-archive' \
    numtide/nix-builder /bin/env /bin/sh -exc "${build_command}"
