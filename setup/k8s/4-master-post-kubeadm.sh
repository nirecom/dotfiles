#!/bin/bash
#
# Install Kubernetes (post kubeadm init)
#
# Install kubectl and download .kube files if not exist
./client.sh

# Observe Taints
echo "Before Untaint:"
kubectl describe nodes | grep -e "Name:" -e "Taints:"

# Untaint: Enable control planes to install pods
echo "Untainging master node ..."
kubectl taint nodes --all node-role.kubernetes.io/master-

# Observe Taints
echo "After Untaint:"
kubectl describe nodes | grep -e "Name:" -e "Taints:"

# See if STATUS is ready
kubectl get nodes
echo If STATUS is "Ready", you finished to install master node!
#echo See https://qiita.com/nykym/items/dcc572c21885543d94c8 for untaint master node if you want.
