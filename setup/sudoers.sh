#!/bin/bash
# Allow the user to run sudo without password
if [ ! -f /etc/sudoers.d/nirecom-users ]; then
    sudo sh -c 'echo "nire    ALL=NOPASSWD: ALL" >>/etc/sudoers.d/nirecom-users'
    sudo chown root:root /etc/sudoers.d/nirecom-users
    sudo chmod 440 /etc/sudoers.d/nirecom-users
fi
