#!/bin/bash
echo "Running kubeadm ..."
sudo kubeadm reset $1
echo "Removing .kube folder ..."
rm -rf $HOME/.kube
