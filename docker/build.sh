#!/bin/sh -ex

LEASTAUTHORITY=${PWD}/$(dirname $(dirname $0))

NIX_STORE=/nix
CACHE_VOLUME=nix-cache-implicit-init

# Allow this option to be overridden in environments which have a version of
# tar too old to support it.  bash -v test is nice but sh only has -z.  Note
# quotes are not required here because the expansion can only result in "" or
# "x".
if [ -z ${EXCLUDE_VCS_IGNORES+x} ]; then
    EXCLUDE_VCS_IGNORES="--exclude-vcs-ignores"
fi

# Get a Nix toolchain environment we can use for the next few steps.
tar cf - -C "${LEASTAUTHORITY}" --exclude-vcs ${EXCLUDE_VCS_IGNORES} . | docker run \
       --rm \
       --interactive \
       --volume ${CACHE_VOLUME}:${NIX_STORE} \
       --volume /var/run/docker.sock:/var/run/docker.sock \
       numtide/nix-builder /bin/env /bin/sh -exc "
           mkdir /leastauthority.com
           tar xf - -C /leastauthority.com
           nix-channel --remove nixpkgs
           nix-channel --add https://nixos.org/channels/nixos-16.09
           nix-channel --update
           /leastauthority.com/docker/_nix-build web grid-router subscription-manager subscription-converger
       "

images="base tahoe-base foolscap-base foolscap-gatherer"
tahoe_images="tahoe-introducer tahoe-storage"

for image in ${images}; do
    docker build -t "leastauthority/${image}" -f "${LEASTAUTHORITY}"/docker/Dockerfile."${image}" "${LEASTAUTHORITY}"
done

for image in ${tahoe_images}; do
    docker build -t "leastauthority/${image}" -f "${LEASTAUTHORITY}"/docker/Dockerfile."${image}" "${LEASTAUTHORITY}"/docker
done
