#!/bin/sh -ex

LEASTAUTHORITY=${PWD}/$(dirname $(dirname $0))

images="base s4-common"

for image in ${images}; do
    docker build -t "leastauthority/${image}" -f "${LEASTAUTHORITY}"/docker/Dockerfile."${image}" "${LEASTAUTHORITY}"
done
