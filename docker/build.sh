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

images="base infrastructure flapp web subscription-manager subscription-converger tahoe-base foolscap-base foolscap-gatherer"
tahoe_images="tahoe-introducer tahoe-storage magicwormhole"

for image in ${images}; do
    docker build -t "leastauthority/${image}" -f "${LEASTAUTHORITY}"/docker/Dockerfile."${image}" "${LEASTAUTHORITY}"
done

for image in ${tahoe_images}; do
    docker build -t "leastauthority/${image}" -f "${LEASTAUTHORITY}"/docker/Dockerfile."${image}" "${LEASTAUTHORITY}"/docker
done
