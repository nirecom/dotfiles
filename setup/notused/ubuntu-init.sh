#!/bin/bash
#if [ `whoami` != "ubuntu" ]; then
#   echo "run with ubuntu"
#fi

# AWS CLI
echo Install AWS CLI .....
if [ ! -d ~/awscli ]; then
    echo Creating directory ~/awscli .....
    mkdir ~/awscli
fi
cd ~/awscli
curl "https://awscli.amazonaws.com/awscli-exe-linux-x86_64.zip" -o "awscliv2.zip"
unzip awscliv2.zip
sudo ./aws/install --update
aws configure

#sudo apt update
#sudo apt -y upgrade

# Allow Agentforwarding to SSH
grep -q -E '^ *AllowAgentForwarding' /etc/ssh/sshd_config
if [ $? -gt 0 ]; then
	echo "Adding AllowAgentForwarding ..."
	sudo sh -c 'echo "#nn Added by sshd.sh" >>/etc/ssh/sshd_config'
    sudo sh -c 'echo "AllowAgentForwarding yes" >>/etc/ssh/sshd_config'
fi
sudo systemctl restart ssh

# Create the user if it does not exist
OPSUSER=nire
getent passwd $OPSUSER >/dev/null
if [ $? -ne 0 ]; then
    echo "Adding user $OPSUSER ..."
    sudo adduser $OPSUSER
    sudo gpasswd -a $OPSUSER sudo
    sudo cp -pr /home/ubuntu/.ssh /home/$OPSUSER/
    sudo chown -R $OPSUSER:$OPSUSER /home/$OPSUSER/.ssh/
fi

# Allow the user to run sudo without password
if [ ! -f /etc/sudoers.d/nirecom-users ]; then
    sudo sh -c 'echo "nire    ALL=NOPASSWD: ALL" >>/etc/sudoers.d/nirecom-users'
    sudo chown root:root /etc/sudoers.d/nirecom-users
    sudo chmod 440 /etc/sudoers.d/nirecom-users
fi

# Copy .ssh/ folder from S3
aws s3 cp --recursive s3://nirecom-home/.ssh ~/.ssh
chmod 600 ~/.ssh/*
chmod +x ~/.ssh/ssh-add-all

