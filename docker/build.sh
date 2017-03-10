#!/bin/sh -ex

nixbuild() {
    nix=$1

    # Build it.
    docker exec "${NIX_BUILDER}" nix-build /leastauthority.com/docker/"${nix}"

    # Get the name of the built artifact.
    artifact=$(docker exec "${NIX_BUILDER}" nix-build /leastauthority.com/docker/"${nix}")

    # Get the built artifact out of the container and load it in to Docker.
    docker exec "${NIX_BUILDER}" cat "${artifact}" | docker load
};

NIX_BUILDER="nix"

if [ "$1" = "" ]; then
    LEASTAUTHORITY=${PWD}/$(dirname $(dirname $0))
else
    LEASTAUTHORITY=$1
    shift
fi

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
if docker run \
       --detach \
       --name "${NIX_BUILDER}" \
       ${CACHE_OPTION} --volume "${LEASTAUTHORITY}":/leastauthority.com \
       numtide/nix-builder \
	   sleep 100000000; then

    # As a horrible hack, copy everything from the cache volume into the Nix
    # store.  The cache volume is separate from the Nix store because the Nix
    # store has some initial state from the container that it would be hard to
    # initialize a volume with.
    docker exec "${NIX_BUILDER}" sh -c "
        if [ -d ${CACHE_PATH} ]; then
            cp -r ${CACHE_PATH}/* /nix/ || /bin/true
        fi
    "
fi

nixbuild "web.nix"
nixbuild "grid-router.nix"

# Copy everything back to the cache volume so it's available for next time.
docker exec "${NIX_BUILDER}" sh -c "cp -r /nix/* ${CACHE_PATH}/"

# Clean up the Nix build container.
docker rm -f "${NIX_BUILDER}"

images="base base-lae-automation infrastructure flapp web subscription-manager subscription-converger tahoe-base foolscap-base foolscap-gatherer"
tahoe_images="tahoe-introducer tahoe-storage magicwormhole"

for image in ${images}; do
    docker build -t "leastauthority/${image}" -f "${LEASTAUTHORITY}"/docker/Dockerfile."${image}" "${LEASTAUTHORITY}"
done

for image in ${tahoe_images}; do
    docker build -t "leastauthority/${image}" -f "${LEASTAUTHORITY}"/docker/Dockerfile."${image}" "${LEASTAUTHORITY}"/docker
done
