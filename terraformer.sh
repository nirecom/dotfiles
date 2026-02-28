#!/bin/bash
source ./bin/detectos.sh

# Install tenv (version manager for Terraform/OpenTofu/Terragrunt)
if type tenv >/dev/null 2>&1; then
    echo "tenv exists. Skip ..."
else
    brew install tenv
fi
echo "Installing terraform..."
tenv terraform install latest
tenv terraform use latest

# Install terraformer
if type terraformer >/dev/null 2>&1; then
    echo "terraformer exists. Skip ..."
else
    echo "Installing terraformer..."
    export PROVIDER=all
    curl -LO https://github.com/GoogleCloudPlatform/terraformer/releases/download/$(curl -s https://api.github.com/repos/GoogleCloudPlatform/terraformer/releases/latest | grep tag_name | cut -d '"' -f 4)/terraformer-${PROVIDER}-linux-amd64
    chmod +x terraformer-${PROVIDER}-linux-amd64
    sudo chown root:root terraformer-${PROVIDER}-linux-amd64
    sudo mv terraformer-${PROVIDER}-linux-amd64 /usr/local/bin/terraformer
fi
