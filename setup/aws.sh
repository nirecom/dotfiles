#!/bin/bash
# AWS CLI
echo Install AWS CLI .....
if [ ! -d ~/awscli ]; then
    echo Creating directory ~/awscli .....
    mkdir ~/awscli
fi
cd ~/awscli
curl "https://awscli.amazonaws.com/awscli-exe-linux-x86_64.zip" -o "awscliv2.zip"
sudo apt install -y unzip
unzip awscliv2.zip
sudo ./aws/install --update

# ECS CLI
# ref. https://qiita.com/homines22/items/bf21b2da9345086b3b3f
echo Install ECS CLI .....
cd ~/awscli
sudo curl -o /usr/local/bin/ecs-cli https://amazon-ecs-cli.s3.amazonaws.com/ecs-cli-linux-amd64-latest
# Verify
sudo apt install -y --no-install-recommends gnupg
gpg --keyserver hkp://keys.gnupg.net --recv BCE9D9A42D51784F
curl -o ecs-cli.asc https://amazon-ecs-cli.s3.amazonaws.com/ecs-cli-linux-amd64-latest.asc
gpg --verify ecs-cli.asc /usr/local/bin/ecs-cli

sudo chmod +x /usr/local/bin/ecs-cli
ecs-cli --version
