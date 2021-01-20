#!/bin/bash
#
# Install Kubernetes (post kubeadm init)
#
# Install kubectl and download .kube files if not exist
./client.sh

# Untaint: Enable control planes to install pods
echo "Untainging master node ..."
kubectl taint nodes --all node-role.kubernetes.io/master-

# See if STATUS is ready
kubectl get nodes
echo If STATUS is "Ready", you finished to install master node!
#echo See https://qiita.com/nykym/items/dcc572c21885543d94c8 for untaint master node if you want.
