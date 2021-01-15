#!/bin/bash
#
# Install Kubernetes
# ref. https://qiita.com/nnagashima/items/d7deb00d086b6e276eea
#
# Run docker-19.03.sh first to match with this version of Kubernetes
echo Install Kubernetes .....
curl -s https://packages.cloud.google.com/apt/doc/apt-key.gpg | sudo apt-key add -
#sudo apt install apt-transport-https curl
# was not required on Ubuntu 20.04
sudo apt update
curl -s https://packages.cloud.google.com/apt/doc/apt-key.gpg | sudo apt-key add
sudo apt-add-repository "deb http://apt.kubernetes.io/ kubernetes-xenial main"
sudo apt install -y kubeadm kubelet kubectl kubernetes-cni
swapoff -a
