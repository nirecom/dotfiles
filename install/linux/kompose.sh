#!/bin/bash
# Install kompose
source ./bin/detectos.sh

if type kompose >/dev/null 2>&1; then
    echo "Already installed."
    exit 1
fi
echo "Installing kompose..."

case "$OSDIST" in
    "ubuntu" )
        cd
        curl -L https://github.com/kubernetes/kompose/releases/download/v1.22.0/kompose-linux-amd64 -o kompose
        chmod +x kompose
        sudo chown root:root kompose
        sudo mv ./kompose /usr/local/bin/kompose
        ;;
    "macos" )
        brew install kompose
        ;;
    * )
        echo "Not supported OS."
        exit 1
esac
