#!/bin/bash
# Install Docker
# ref. https://kubernetes.io/docs/setup/production-environment/container-runtimes/
echo Install Docker .....
sudo apt-get update && sudo apt-get install -y \
  apt-transport-https ca-certificates curl software-properties-common gnupg2
curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo apt-key --keyring /etc/apt/trusted.gpg.d/docker.gpg add -
sudo add-apt-repository \
  "deb [arch=amd64] https://download.docker.com/linux/ubuntu \
  $(lsb_release -cs) \
  stable"
# Install Docker CE
sudo apt-get update && sudo apt-get install -y --allow-downgrades \
  containerd.io=1.2.13-2 \
  docker-ce=5:19.03.11~3-0~ubuntu-$(lsb_release -cs) \
  docker-ce-cli=5:19.03.11~3-0~ubuntu-$(lsb_release -cs)
# Set up the Docker daemon
cat <<EOF | sudo tee /etc/docker/daemon.json
{
  "exec-opts": ["native.cgroupdriver=systemd"],
  "log-driver": "json-file",
  "log-opts": {
    "max-size": "100m"
  },
  "storage-driver": "overlay2"
}
EOF
sudo mkdir -p /etc/systemd/system/docker.service.d
# Restart Docker
sudo systemctl daemon-reload
sudo systemctl restart docker
# Start Docker on boot
sudo systemctl enable docker
# Install Kubernetes
# ref. https://qiita.com/nnagashima/items/d7deb00d086b6e276eea
echo Install Kubernetes .....
curl -s https://packages.cloud.google.com/apt/doc/apt-key.gpg | sudo apt-key add -
#sudo apt install apt-transport-https curl
# was not required on Ubuntu 20.04
sudo apt update
sudo apt install kubeadm
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
