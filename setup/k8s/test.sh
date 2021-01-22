#!/bin/bash
export HOST0=k8s-master-ne1a
export HOST1=k8s-master-ne1c
export HOST2=k8s-master-ne1d

USER=nire
SSHHOSTS=(${HOST1} ${HOST2})
TARFILE=kubernetes-pki.tar
for i in "${!SSHHOSTS[@]}"; do
  HOST=${SSHHOSTS[$i]}
  echo "Creating & Copying tarball to $HOST ..."
  sudo sh -c "cd /tmp/${HOST}; tar cf /tmp/${TARFILE} ./pki"
  scp -r /tmp/${TARFILE} ${USER}@${HOST}:

  # clean up certs that should not be copied off this host
  sudo find /tmp/${HOST} -name ca.key -type f -delete
  sudo rm /tmp/${TARFILE}
  
done
echo "Run following commands manually:"
echo "---"
echo "cd /etc/kubernetes; sudo tar xf ~/${TARFILE}; sudo chown -R root:root /etc/kubernetes"
echo "---"
