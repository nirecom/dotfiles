#!/bin/bash
#
# Install Kubernetes
# ref. https://qiita.com/nnagashima/items/d7deb00d086b6e276eea

# Letting iptables see bridged traffic
# ref. https://kubernetes.io/docs/setup/production-environment/tools/kubeadm/install-kubeadm/#troubleshooting
cat <<EOF | sudo tee /etc/modules-load.d/k8s.conf
br_netfilter
EOF

cat <<EOF | sudo tee /etc/sysctl.d/k8s.conf
net.bridge.bridge-nf-call-ip6tables = 1
net.bridge.bridge-nf-call-iptables = 1
EOF

sudo sysctl --system

if type "kubeadm" >/dev/null 2>&1; then
    echo kubadm exists. Skipping installation of Kubernetes tools...
else
    echo Install Kubernetes .....
    sudo apt update
    sudo apt install -y apt-transport-https curl        
    curl -s https://packages.cloud.google.com/apt/doc/apt-key.gpg | sudo apt-key add -
    sudo apt-add-repository "deb http://apt.kubernetes.io/ kubernetes-xenial main"
    sudo apt install -y kubeadm kubelet kubectl kubernetes-cni
fi
swapoff -a

#
# Install etcd: Common Part
#
cat << EOF | sudo tee /etc/systemd/system/kubelet.service.d/20-etcd-service-manager.conf
[Service]
ExecStart=
#  Replace "systemd" with the cgroup driver of your container runtime. The default value in the kubelet is "cgroupfs".
ExecStart=/usr/bin/kubelet --address=127.0.0.1 --pod-manifest-path=/etc/kubernetes/manifests --cgroup-driver=systemd
Restart=always
EOF

sudo systemctl daemon-reload
sudo systemctl restart kubelet
