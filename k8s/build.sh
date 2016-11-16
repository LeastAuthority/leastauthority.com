# K8S_POD tells us which k8s pod in which to build.
# GIT_BRANCH tells us which leastauthority.com branch to build.
# DOCKER_TAG tells us what tag to apply to the build images.

[ -v K8S_POD && -v GIT_BRANCH && -v DOCKER_TAG ] || {
    echo "Set K8S_POD, GIT_BRANCH, and DOCKER_TAG variables."
    exit 1
}

# A helper to run commands inside a container in the pod.
EXEC="kubectl exec -i ${K8S_POD} --"

# First synchronize the secrets to the container.
rsync \
    --delete \
    --exclude-from .gitignore \
    --exclude-from .dockerignore \
    -avzP \
    --blocking-io \
    --rsh="${EXEC} -- rsync " \
    ./secret_config/ \
    :/root/secret_config/

${EXEC} bash -e -x -c '
    # Install build dependencies.
    apt-get update && apt-get install -y git docker.io python-twisted;

    # Do the build in /root, why not?
    cd /root;

    # Clean up any old source checkout
    rm -rf leastauthority.com

    # Get a new source checkout.
    # Get a shallow one to save time/bandwidth.
    # Get the branch the user requested.
    git clone --depth 1 --branch "${GIT_BRANCH}" http://github.com/leastauthority/leastauthority.com;

    # Put the secrets in the place expected by the build.
    cp -a /root/secret_config leastauthority.com;

    # Build the images.
    ./leastauthority.com/docker/build.sh;

    # Tag them in the way expected by the deployment configuration and
    # with the tag given in the environment.
    docker tag leastauthority/web 127.0.0.1:30000/leastauthority/web:"${DOCKER_TAG}"
    docker tag leastauthority/flapp 127.0.0.1:30000/leastauthority/flapp:"${DOCKER_TAG}"

    # Forward a local port to the private registry so we can push the new images.
    twistd portforward --port 30000 --host private-registry.leastauthority-tweaks --dest_port 30000

    # And push them.
    docker push 127.0.0.1:30000/leastauthority/web:"${DOCKER_TAG}"
    docker push 127.0.0.1:30000/leastauthority/flapp:"${DOCKER_TAG}"
'
