#!/bin/bash
if type go >/dev/null 2>&1; then
    echo "it's installed."
    exit 1
fi
cd /tmp
wget https://golang.org/dl/go1.15.7.linux-amd64.tar.gz
#sudo tar -C /usr/local -xzf go1.15.7.linux-amd64.tar.gz
#export PATH=$PATH:/usr/local/go/bin
#go version
