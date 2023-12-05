#!/bin/bash

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
ROOT_DIR="$(dirname "$SCRIPT_DIR")"
BIN_DIR="$ROOT_DIR/bin"
DOCKER_IMAGE="grace-compiler"
DOCKERFILE_PATH="$ROOT_DIR"

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

GRACEC_PATH="$BIN_DIR/gracec"

if [ -e "$GRACEC_PATH" ]; then
    echo "Removing existing 'gracec' file..."
    rm "$GRACEC_PATH"
fi

echo "#!/bin/bash" > "$GRACEC_PATH"
echo "$CONTAINER_RUNTIME run -i $DOCKER_IMAGE ./bin/gracec" >> "$GRACEC_PATH"
chmod +x "$GRACEC_PATH"

echo "The 'gracec' file has been created in the bin directory."

echo "Done."

