#!/bin/sh

LEASTAUTHORITY=$(dirname $(dirname $0))

docker build -t leastauthority/infrastructure -f "${LEASTAUTHORITY}"/docker/Dockerfile.infrastructure "${LEASTAUTHORITY}"
docker build -t leastauthority/flapp -f "${LEASTAUTHORITY}"/docker/Dockerfile.flapp "${LEASTAUTHORITY}"
docker build -t leastauthority/web -f "${LEASTAUTHORITY}"/docker/Dockerfile.web "${LEASTAUTHORITY}"

docker build -t leastauthority/magicwormhole -f "${LEASTAUTHORITY}"/docker/Dockerfile.magicwormhole-relay "${LEASTAUTHORITY}"/docker
