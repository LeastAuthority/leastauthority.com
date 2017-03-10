#!/bin/sh -ex

LEASTAUTHORITY=${PWD}/$(dirname $(dirname $0))

CACHE_PATH=/nix-cache
CACHE_VOLUME=nix-cache

if docker volume ls >/dev/null 2>&1; then
    # A cache for build artifacts.  If we created it on a previous run, great.
    docker volume create --name "${CACHE_VOLUME}" || /bin/true
    CACHE_OPTION="--volume ${CACHE_VOLUME}:${CACHE_PATH}"
else
    CACHE_OPTION=""
fi

# Get a Nix toolchain environment we can use for the next few steps.
tar cf - -C "${LEASTAUTHORITY}" . | docker run \
       --rm \
       --interactive \
       ${CACHE_OPTION} \
       --volume "${LEASTAUTHORITY}":/leastauthority.com \
       --volume /var/run/docker.sock:/var/run/docker.sock \
       numtide/nix-builder /bin/env CACHE_PATH=${CACHE_PATH} /bin/sh -exc '
           tar xf - -C /leastauthority.com

           # As a horrible hack, copy everything from the cache volume into
           # the Nix store.  The cache volume is separate from the Nix store
           # because the Nix store has some initial state from the container
           # that it would be hard to initialize a volume with.
           if [ -d ${CACHE_PATH} ]; then
               cp -r ${CACHE_PATH}/* /nix/ || /bin/true
           fi

           nixbuild() {
               # Build it.
               nix-build /leastauthority.com/docker/$1

               # Get the name of the built artifact.
               artifact=$(nix-build /leastauthority.com/docker/$1)

               # Get the built artifact out of the container and load it in to
               # Docker.
               docker load --input ${artifact}
           }

           nixbuild web.nix
           nixbuild grid-router.nix

           # Copy everything back to the cache volume so it is available
           # for next time.
           if [ -d ${CACHE_PATH} ]; then
               cp -r /nix/* ${CACHE_PATH}/
           fi
       '

images="base base-lae-automation infrastructure flapp web subscription-manager subscription-converger tahoe-base foolscap-base foolscap-gatherer"
tahoe_images="tahoe-introducer tahoe-storage magicwormhole"

for image in ${images}; do
    docker build -t "leastauthority/${image}" -f "${LEASTAUTHORITY}"/docker/Dockerfile."${image}" "${LEASTAUTHORITY}"
done

for image in ${tahoe_images}; do
    docker build -t "leastauthority/${image}" -f "${LEASTAUTHORITY}"/docker/Dockerfile."${image}" "${LEASTAUTHORITY}"/docker
done
