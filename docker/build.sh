#!/bin/sh

LEASTAUTHORITY=$(dirname $(dirname $0))

docker build -t leastauthority.com/infrastructure -f "${LEASTAUTHORITY}"/docker/Dockerfile.infrastructure "${LEASTAUTHORITY}"
docker build -t leastauthority.com/flappserver -f "${LEASTAUTHORITY}"/docker/Dockerfile.flappserver "${LEASTAUTHORITY}"
docker build -t leastauthority.com/web -f "${LEASTAUTHORITY}"/docker/Dockerfile.web "${LEASTAUTHORITY}"
