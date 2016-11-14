#!/bin/sh

LEASTAUTHORITY=$(dirname $(dirname $0))

docker build -t leastauthority.com/base -f "${LEASTAUTHORITY}"/docker/Dockerfile.base "${LEASTAUTHORITY}"

docker build -t leastauthority.com/infrastructure -f "${LEASTAUTHORITY}"/docker/Dockerfile.infrastructure "${LEASTAUTHORITY}"
docker build -t leastauthority.com/flapp -f "${LEASTAUTHORITY}"/docker/Dockerfile.flapp "${LEASTAUTHORITY}"
docker build -t leastauthority.com/web -f "${LEASTAUTHORITY}"/docker/Dockerfile.web "${LEASTAUTHORITY}"

EMPTY="$(mktemp -d)"
docker build -t leastauthority.com/tahoe-base -f "${LEASTAUTHORITY}"/docker/Dockerfile.tahoe-base "${LEASTAUTHORITY}"/docker
docker build -t leastauthority.com/tahoe-introducer -f "${LEASTAUTHORITY}"/docker/Dockerfile.tahoe-introducer "${LEASTAUTHORITY}"/docker
docker build -t leastauthority.com/tahoe-storage -f "${LEASTAUTHORITY}"/docker/Dockerfile.tahoe-storage "${LEASTAUTHORITY}"/docker
