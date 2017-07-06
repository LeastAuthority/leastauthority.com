#!/bin/sh -ex

LEASTAUTHORITY=${PWD}/$(dirname $(dirname $0))

images="base s4-common tahoe-base foolscap-base foolscap-gatherer"
tahoe_images="tahoe-introducer tahoe-storage"

for image in ${images}; do
    docker build -t "leastauthority/${image}" -f "${LEASTAUTHORITY}"/docker/Dockerfile."${image}" "${LEASTAUTHORITY}"
done

for image in ${tahoe_images}; do
    docker build -t "leastauthority/${image}" -f "${LEASTAUTHORITY}"/docker/Dockerfile."${image}" "${LEASTAUTHORITY}"/docker
done
