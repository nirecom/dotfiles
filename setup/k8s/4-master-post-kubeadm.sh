#!/bin/bash
#
# Install Kubernetes (post kubeadm init)
#
# Apply Flannel
#echo "Applying Flannel ..."
#kubectl apply -f https://raw.githubusercontent.com/coreos/flannel/master/Documentation/kube-flannel.yml

# Apply Weave Net
#echo "Applying Weave Net ..."
kubectl apply -f "https://cloud.weave.works/k8s/net?k8s-version=$(kubectl version | base64 | tr -d '\n')"

# Untaint: Enable control planes to install pods
#echo "Untainging master node ..."
#kubectl taint nodes --all node-role.kubernetes.io/master-
# See if STATUS is ready
kubectl get nodes
echo If STATUS is "Ready", you finished to install master node!
echo See https://qiita.com/nykym/items/dcc572c21885543d94c8 for untaint master node if you want.
