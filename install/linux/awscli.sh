#!/bin/bash
source ./bin/detectos.sh

# Install AWS CLI
if type aws >/dev/null 2>&1; then
    echo "awscli exists. Skipping install..."
else
    case "$OSDIST" in
        "mingw" )
            echo "MINGW is not supported. Please install Windows installer manually."
            exit 1
            ;;
        "macos" )
#            brew install awscli
            cd
            AWSPKG=/tmp/AWSCLIV2.pkg
            curl -L https://awscli.amazonaws.com/AWSCLIV2.pkg -o $AWSPKG
            sudo installer -pkg $AWSPKG -target "/Volumes/Macintosh HD"
            rm $AWSPKG
            ;;
        "amazon" )
            echo "no need to install awscli."
            ;;
        "ubuntu" ) 
            if [ ! -d ~/awscli ]; then
                echo Creating directory ~/awscli .....
                mkdir ~/awscli
            fi
            cd ~/awscli
            if type aws >/dev/null 2>&1; then
                echo "aws command exists. Skipping install ..."
            else
                echo "Installing awscli ..."
                curl "https://awscli.amazonaws.com/awscli-exe-linux-x86_64.zip" -o "awscliv2.zip"
                sudo apt install -y unzip
                unzip awscliv2.zip
                sudo ./aws/install --update
            fi
    esac
fi

if [ -e $HOME/.aws/credentials ]; then
    echo "awscli is already configured. Skip aws configure..."
else
    aws configure
fi

# ECS CLI
# ref. https://qiita.com/homines22/items/bf21b2da9345086b3b3f
if type ecs-cli >/dev/null 2>&1; then
    echo "ecs-cli exists. Skipping install ..."
else
    echo "Install ecs-cli ..."
#    cd ~/awscli
    case "$OSDIST" in
        "macos" )
            sudo curl -Lo /usr/local/bin/ecs-cli https://amazon-ecs-cli.s3.amazonaws.com/ecs-cli-darwin-amd64-latest
            # brew install gnupg # M1 mac seems not supported. skip.
            ;;
        "ubuntu" )
            sudo curl -o /usr/local/bin/ecs-cli https://amazon-ecs-cli.s3.amazonaws.com/ecs-cli-linux-amd64-latest
            sudo apt install -y --no-install-recommends gnupg
            gpg --keyserver hkp://keys.gnupg.net --recv BCE9D9A42D51784F
            curl -o ecs-cli.asc https://amazon-ecs-cli.s3.amazonaws.com/ecs-cli-linux-amd64-latest.asc
            gpg --verify ecs-cli.asc /usr/local/bin/ecs-cli
            ;;
        "amazon" )
            sudo curl -o /usr/local/bin/ecs-cli https://amazon-ecs-cli.s3.amazonaws.com/ecs-cli-linux-amd64-latest
            sudo yum install -y gnupg
            gpg --keyserver hkp://keys.gnupg.net --recv BCE9D9A42D51784F
            curl -o ecs-cli.asc https://amazon-ecs-cli.s3.amazonaws.com/ecs-cli-linux-amd64-latest.asc
            gpg --verify ecs-cli.asc /usr/local/bin/ecs-cli
            ;;
    esac
    sudo chmod +x /usr/local/bin/ecs-cli
    ecs-cli --version
fi
