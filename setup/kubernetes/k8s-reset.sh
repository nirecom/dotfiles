#!/bin/bash
echo "Running kubeadm ..."
kubeadm reset
echo "Removing .kube folder ..."
rm -rf $HOME/.kube
