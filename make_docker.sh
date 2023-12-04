#!/bin/bash

DOCKER_IMAGE="grace-compiler"
DOCKERFILE_PATH="."

DEFAULT_RUNTIME="docker"

if [ "$#" -ge 1 ]; then
    if [ "$1" == "podman" ]; then
        CONTAINER_RUNTIME="podman"
    else
        CONTAINER_RUNTIME="docker"
    fi
else
    CONTAINER_RUNTIME="$DEFAULT_RUNTIME"
fi

image_exists() {
    if [ "$CONTAINER_RUNTIME" == "docker" ]; then
        docker image inspect "$1" > /dev/null 2>&1
    else
        podman image exists "$1"
    fi
}

if ! image_exists "$DOCKER_IMAGE"; then
    echo "Building Docker image..."
    $CONTAINER_RUNTIME build -t "$DOCKER_IMAGE" "$DOCKERFILE_PATH"
fi


if [ -e "gracec" ]; then
    echo "Removing existing 'gracec' file..."
    rm "gracec"
fi

echo "#!/bin/bash" > gracec
echo "$CONTAINER_RUNTIME run -i $DOCKER_IMAGE ./gracec" >> gracec
chmod +x gracec

echo "The 'gracec' file has been created."

echo "Done."

