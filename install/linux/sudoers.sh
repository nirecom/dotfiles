#!/bin/bash
source ./bin/detectos.sh

FILENAME="/etc/sudoers.d/nirecom-users"
if [ ! -f /etc/sudoers.d/nirecom-users ]; then
    echo "Need to allow the user to run sudo without password."
    echo "Configuretion *may* require to enter password once."

    sudo sh -c 'echo "nire    ALL=(ALL) NOPASSWD: ALL" >/etc/sudoers.d/nirecom-users'
    sudo chmod 440 $FILENAME
    if [ "$OSDIST" = "macos" ]; then
        sudo chown root:wheel $FILENAME
    elif [ "$OSDIST" = "ubuntu" ] || [ "$OSDIST" = "amazon" ]; then
        sudo chown root:root $FILENAME
    fi
fi
