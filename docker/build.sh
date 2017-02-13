#!/bin/sh -ex

NIX_BUILDER="nix"

LEASTAUTHORITY=$(dirname $(dirname $0))
PARENT=${LEASTAUTHORITY}/..

CACHE_PATH=/nix-cache
CACHE_VOLUME=nix-cache

# A cache for build artifacts.  If we created it on a previous run, great.
docker volume create --name "${CACHE_VOLUME}" || /bin/true

# Get a Nix toolchain environment we can use for the next few steps.  Allow
# this to fail and suppose that the reason for failure is that it is already
# running.
docker run \
       --detach \
       --name "${NIX_BUILDER}" \
       --volume "${CACHE_VOLUME}":"${CACHE_PATH}" \
       numtide/nix-builder \
	   sleep 100000000 \
|| /bin/true

# To facilitate leaving the nix builder running across multiple builds, copy
# the source into the container like this instead of doing something with a
# Docker volume.
docker exec "${NIX_BUILDER}" rm -rf /leastauthority.com
tar cf - --directory "${PARENT}" "./leastauthority.com" | \
    docker exec -i "${NIX_BUILDER}" tar xvf -

# As a horrible hack, copy everything from the cache volume into the Nix
# store.  The cache volume is separate from the Nix store because the Nix
# store has some initial state from the container that it would be hard to
# initialize a volume with.
docker exec "${NIX_BUILDER}" sh -c "cp -r ${CACHE_PATH}/* /nix/ || /bin/true"

# Build it.
docker exec "${NIX_BUILDER}" nix-build /leastauthority.com/docker/web.nix

# Get the name of the built artifact.
web=$(docker exec "${NIX_BUILDER}" nix-build /leastauthority.com/docker/web.nix)

# Copy everything back to the cache volume so it's available for next time.
docker exec "${NIX_BUILDER}" sh -c "cp -r /nix/* ${CACHE_PATH}/"

# Get the built artifact out of the container and load it in to Docker.
docker exec "${NIX_BUILDER}" cat "${web}" | docker load

# Clean up the Nix build container.
docker rm -f "${NIX_BUILDER}"

docker build -t leastauthority/base -f "${LEASTAUTHORITY}"/docker/Dockerfile.base "${LEASTAUTHORITY}"

docker build -t leastauthority/infrastructure -f "${LEASTAUTHORITY}"/docker/Dockerfile.infrastructure "${LEASTAUTHORITY}"
docker build -t leastauthority/flapp -f "${LEASTAUTHORITY}"/docker/Dockerfile.flapp "${LEASTAUTHORITY}"

docker build -t leastauthority/subscription-manager -f "${LEASTAUTHORITY}"/docker/Dockerfile.subscription-manager "${LEASTAUTHORITY}"

docker build -t leastauthority/tahoe-base -f "${LEASTAUTHORITY}"/docker/Dockerfile.tahoe-base "${LEASTAUTHORITY}"
docker build -t leastauthority/tahoe-introducer -f "${LEASTAUTHORITY}"/docker/Dockerfile.tahoe-introducer "${LEASTAUTHORITY}"/docker
docker build -t leastauthority/tahoe-storage -f "${LEASTAUTHORITY}"/docker/Dockerfile.tahoe-storage "${LEASTAUTHORITY}"/docker

docker build -t leastauthority/foolscap-base -f "${LEASTAUTHORITY}"/docker/Dockerfile.foolscap-base "${LEASTAUTHORITY}"
docker build -t leastauthority/foolscap-gatherer -f "${LEASTAUTHORITY}"/docker/Dockerfile.foolscap-gatherer "${LEASTAUTHORITY}"

docker build -t leastauthority/magicwormhole -f "${LEASTAUTHORITY}"/docker/Dockerfile.magicwormhole-relay "${LEASTAUTHORITY}"/docker
