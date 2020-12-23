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
sudo apt install kubeadm kubelet kubectl kubernetes-cni
swapoff -a
sudo kubeadm init --pod-network-cidr=10.244.0.0/16
# You will see Docker version error with 20.0.1. Need to use 19.03
# ref. https://qiita.com/soumi/items/7736ac3aabbbe4fb474a
# If succeeded, you will see messages:
# "Your Kubernetes control-plane has initialized successfully!"

# Copy Kubernetes config files as instructed
mkdir -p $HOME/.kube
sudo cp -i /etc/kubernetes/admin.conf $HOME/.kube/config
sudo chown $(id -u):$(id -g) $HOME/.kube/config

# Edit iptables for flannel
sudo sysctl net.bridge.bridge-nf-call-iptables=1
# Apply Flannel
kubectl apply -f https://raw.githubusercontent.com/coreos/flannel/master/Documentation/kube-flannel.yml
# See if STATUS is ready
kubectl get nodes
echo If STATUS is "Ready", you finished to install master node!
echo See https://qiita.com/nykym/items/dcc572c21885543d94c8 for untaint master node if you want.
