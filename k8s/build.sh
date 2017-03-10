#!/bin/bash -ex

# K8S_POD tells us which k8s pod in which to build.

[[ -v K8S_POD ]] || {
    # Find it.
    K8S_POD=$(kubectl --context prod -o json get pods -l run=image-building | jq -r '.items[0].metadata.name')
}

REPO="$(dirname $0)/../.git"

# A helper to run commands inside a container in the pod.
EXEC="kubectl exec  --context prod -i ${K8S_POD} --"

${EXEC} bash -e -x -c '
    # Dependencies for the build.
    DEPS="git docker.io python-twisted"

    # If any of them are not installed, install them.
    # This lets us skip the slightly slow "apt-get update" on subsequent runs.
    if ! dpkg --status ${DEPS} >/dev/null 2>&1; then
        apt-get update
        apt-get install -y ${pkg}
    fi
'

# Find the git revision hash that we're on right now.  Use it as the Docker
# image tag so that the image can be unambiguously associated with a revision
# of the software.
DOCKER_TAG=$(git --git-dir "${REPO}" rev-parse --short HEAD)

${EXEC} env DOCKER_TAG=${DOCKER_TAG} bash -ex -c '
    DOCKER_TAG="'"${DOCKER_TAG}"'"

    # Get a fresh, clean space to work in.
    WORKDIR=$(mktemp -d)

    SERVER_PORT=30000
    SERVER="127.0.0.1:${SERVER_PORT}"

    microservices="web flapp tahoe-introducer tahoe-storage foolscap-gatherer magicwormhole subscription-manager subscription-converger"

    # Get back to our temporary working directory.
    pushd "${WORKDIR}"

    # Get a new source checkout.
    # XXX Get a shallow one to save time/bandwidth.
    REPO="http://github.com/leastauthority/leastauthority.com"
    git clone http://github.com/leastauthority/leastauthority.com
    pushd leastauthority.com
    git checkout "${DOCKER_TAG}"
    popd

    # Build the images.
    ./leastauthority.com/docker/build.sh

    # Tag them in the way expected by the deployment configuration and
    # with the tag given in the environment.
    for microservice in ${microservices}; do
        repo="leastauthority/${microservice}"
        docker tag ${repo} "${SERVER}/${repo}:${DOCKER_TAG}"
    done

    # Clean up the last portforwarder, if necessary.
    [ -e /tmp/portforward.pid ] && kill $(cat /tmp/portforward.pid) || /bin/true

    # Forward a local port to the private registry so we can push the new images.
    twistd \
        --pidfile /tmp/portforward.pid \
        portforward \
            --port 30000 \
            --host private-registry.leastauthority-tweaks \
            --dest_port ${SERVER_PORT}

    # And push them.
    for microservice in ${microservices}; do
        repo="leastauthority/${microservice}"
        docker push "${SERVER}/${repo}:${DOCKER_TAG}"
    done

    echo "Tagged images with ${DOCKER_TAG}"
'
