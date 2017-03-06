#!/bin/sh -ex

NIX_BUILDER="nix"

SELF=$(realpath $0)
LEASTAUTHORITY=$(dirname $(dirname ${SELF}))
PARENT=$(dirname ${LEASTAUTHORITY})

# This will outlast the script.  That's okay.  We're using it as a cache,
# effectively.  Pretty terrible way to set things up.  Maybe I'll improve it
# later.
docker run \
       --detach \
       --name "${NIX_BUILDER}" \
       --restart always \
       --volume ${PARENT}:/parent \
       numtide/nix-builder \
	   sh -c 'ln -s /parent/leastauthority.com /leastauthority.com && sleep 100000000' \
|| /bin/true

docker exec "${NIX_BUILDER}" nix-build /leastauthority.com/docker/web.nix
web=$(docker exec "${NIX_BUILDER}" nix-build /leastauthority.com/docker/web.nix)
docker exec "${NIX_BUILDER}" cat "${web}" | docker load

docker build -t leastauthority/base -f "${LEASTAUTHORITY}"/docker/Dockerfile.base "${LEASTAUTHORITY}"

docker build -t leastauthority/infrastructure -f "${LEASTAUTHORITY}"/docker/Dockerfile.infrastructure "${LEASTAUTHORITY}"
docker build -t leastauthority/flapp -f "${LEASTAUTHORITY}"/docker/Dockerfile.flapp "${LEASTAUTHORITY}"

docker build -t leastauthority/tahoe-base -f "${LEASTAUTHORITY}"/docker/Dockerfile.tahoe-base "${LEASTAUTHORITY}"
docker build -t leastauthority/tahoe-introducer -f "${LEASTAUTHORITY}"/docker/Dockerfile.tahoe-introducer "${LEASTAUTHORITY}"/docker
docker build -t leastauthority/tahoe-storage -f "${LEASTAUTHORITY}"/docker/Dockerfile.tahoe-storage "${LEASTAUTHORITY}"/docker

docker build -t leastauthority/foolscap-base -f "${LEASTAUTHORITY}"/docker/Dockerfile.foolscap-base "${LEASTAUTHORITY}"
docker build -t leastauthority/foolscap-gatherer -f "${LEASTAUTHORITY}"/docker/Dockerfile.foolscap-gatherer "${LEASTAUTHORITY}"

docker build -t leastauthority/magicwormhole -f "${LEASTAUTHORITY}"/docker/Dockerfile.magicwormhole-relay "${LEASTAUTHORITY}"/docker
