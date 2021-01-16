#!/bin/bash
echo "Running kubeadm ..."
sudo kubeadm reset
echo "Removing .kube folder ..."
rm -rf $HOME/.kube
