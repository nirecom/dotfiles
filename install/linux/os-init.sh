#!/bin/bash
# Initialize Ubuntu
source ./bin/detectos.sh

# Check current user name
USERNAME=nire
case "$OSDIST" in
    "ubuntu" )
        if [ `whoami` != "ubuntu" ]; then
            echo "Run with user: ubuntu. Abort."
            exit 1
        fi
        ;;
    "amazon" )
        if [ `whoami` != "ec2-user" ]; then
            echo "Run with user: ec2-user. Abort."
            exit 1
        fi
esac

# Update Packages, Create $USERNAME
case "$OSDIST" in
    "ubuntu" )
        sudo apt update && sudo apt -y upgrade
        sudo apt install -y language-pack-ja-base language-pack-ja
        #sudo apt ibus-mozc
        if ! $ISWSL; then
            sudo timedatectl set-timezone Asia/Tokyo
        fi
        ./sshd.sh
        if ! getent passwd $USERNAME >/dev/null 2>&1; then
            echo "Creating user $USERNAME ..."
            sudo adduser $USERNAME
            sudo gpasswd -a $USERNAME sudo
            sudo cp -pr ~/.ssh /home/$USERNAME/
            sudo chown -R $USERNAME:$USERNAME /home/$USERNAME/.ssh/
        fi
        ;;
    "amazon" )
        sudo yum -y update
        ./sshd.sh
        if ! getent passwd $USERNAME >/dev/null 2>&1; then
            echo "Creating user $USERNAME ..."
            sudo useradd $USERNAME
            sudo usermod -G wheel $USERNAME
            sudo cp -pr ~/.ssh /home/$USERNAME/
            sudo chown -R $USERNAME:$USERNAME /home/$USERNAME/.ssh/
            echo "Set password of $USERNAME..."
            sudo passwd $USERNAME
        fi
esac

# Added to sudoers
./sudoers.sh

