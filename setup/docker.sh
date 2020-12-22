#!/bin/bash
# install docker
# ref. https://docs.docker.com/engine/install/ubuntu/
# TODO: Latest docker version does not work with Kubernetes. See setup-kubernetes.sh
echo "Install docker ....."
sudo apt-get update
sudo apt-get install \
    apt-transport-https \
    ca-certificates \
    curl \
    gnupg-agent \
    software-properties-common
curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo apt-key add -
sudo apt-key fingerprint 0EBFCD88

if [ `arch` = "x86_64" ]; then
    echo Configure x86_64 ......
    sudo add-apt-repository \
         "deb [arch=amd64] https://download.docker.com/linux/ubuntu \
            $(lsb_release -cs) \
               stable"
fi

sudo apt-get update
sudo apt-get install docker-ce docker-ce-cli containerd.io

# install docker-compose
# ref. https://docs.docker.com/engine/install/ubuntu/
echo "Install docker-compose ....."
sudo apt install docker-compose

# set $USER to docker group
sudo gpasswd -a $USER docker
echo Added $USER to docker group. Please re-login shell.
