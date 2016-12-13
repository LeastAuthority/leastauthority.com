#!/bin/bash -ex

# K8S_POD tells us which k8s pod in which to build.
# GIT_BRANCH tells us which leastauthority.com branch to build.

[[ -v K8S_POD && -v GIT_BRANCH ]] || {
    echo "Set K8S_POD and GIT_BRANCH variables."
    exit 1
}

# A helper to run commands inside a container in the pod.
EXEC="kubectl exec -i ${K8S_POD} --"

${EXEC} bash -e -x -c '
    # Dependencies for the build.
    DEPS="git docker.io python-twisted"

    # If any of them are not installed, install them.
    # This lets us skip the slightly slow "apt-get update" on subsequent runs.
    dpkg --status ${DEPS} >/dev/null 2>&1 || apt-get update && apt-get install -y ${DEPS};
'

# Get a fresh, clean space to work in.
TEMP=$(${EXEC} bash -c 'mktemp -d')

${EXEC} bash -ex -c '
    # Clone the git branch variable to the remote environment.
    GIT_BRANCH="'"${GIT_BRANCH}"'"
    WORKDIR="'"${TEMP}"'"

    # Get back to our temporary working directory.
    pushd "${WORKDIR}"

    # Get a new source checkout.
    # Get a shallow one to save time/bandwidth.
    # Get the branch the user requested.
    git clone --depth 1 --branch "${GIT_BRANCH}" http://github.com/leastauthority/leastauthority.com;

    # Find the git revision hash that was just checked out.
    # Use it as the Docker image tag so that the image can be
    # unambiguously associated with a revision of the software.
    DOCKER_TAG=$(git --git-dir leastauthority.com/.git rev-parse --short HEAD)

    # Build the images.
    ./leastauthority.com/docker/build.sh;

    # Tag them in the way expected by the deployment configuration and
    # with the tag given in the environment.
    docker tag leastauthority/web 127.0.0.1:30000/leastauthority/web:"${DOCKER_TAG}"
    docker tag leastauthority/flapp 127.0.0.1:30000/leastauthority/flapp:"${DOCKER_TAG}"

    docker tag leastauthority/tahoe-introducer 127.0.0.1:30000/leastauthority/tahoe-introducer:"${DOCKER_TAG}"
    docker tag leastauthority/tahoe-storage 127.0.0.1:30000/leastauthority/tahoe-storage:"${DOCKER_TAG}"

    docker tag leastauthority/foolscap-gatherer 127.0.0.1:30000/leastauthority/foolscap-gatherer:"${DOCKER_TAG}"

    docker tag leastauthority/magicwormhole 127.0.0.1:30000/leastauthority/magicwormhole:"${DOCKER_TAG}"

    # Clean up the last portforwarder, if necessary.
    [ -e /tmp/portforward.pid ] && kill $(cat /tmp/portforward.pid) || /bin/true

    # Forward a local port to the private registry so we can push the new images.
    twistd \
        --pidfile /tmp/portforward.pid \
        portforward \
            --port 30000 \
            --host private-registry.leastauthority-tweaks \
            --dest_port 30000

    # And push them.
    docker push 127.0.0.1:30000/leastauthority/web:"${DOCKER_TAG}"
    docker push 127.0.0.1:30000/leastauthority/flapp:"${DOCKER_TAG}"
    docker push 127.0.0.1:30000/leastauthority/tahoe-introducer:"${DOCKER_TAG}"
    docker push 127.0.0.1:30000/leastauthority/tahoe-storage:"${DOCKER_TAG}"
    docker push 127.0.0.1:30000/leastauthority/foolscap-gatherer:"${DOCKER_TAG}"
    docker push 127.0.0.1:30000/leastauthority/magicwormhole:"${DOCKER_TAG}"

    echo "Tagged images with ${DOCKER_TAG}"
'
