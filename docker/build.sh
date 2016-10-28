#!/bin/sh

LEASTAUTHORITY=$(dirname $(dirname $0))

docker build -t leastauthority.com/infrastructure -f leastauthority.com/docker/Dockerfile.infrastructure "${LEASTAUTHORITY}"
docker build -t leastauthority.com/flappserver -f leastauthority.com/docker/Dockerfile.flappserver "${LEASTAUTHORITY}"
docker build -t leastauthority.com/web -f leastauthority.com/docker/Dockerfile.web "${LEASTAUTHORITY}"
