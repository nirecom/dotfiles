#!/bin/bash
#
# Install Kubernetes
# Part2 - master only
# ref. https://qiita.com/nnagashima/items/d7deb00d086b6e276eea
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
