#!/bin/bash
# Allow Agentforwarding to SSH
grep -q -E '^ *AllowAgentForwarding' /etc/ssh/sshd_config
if [ $? -gt 0 ]; then
	echo "Adding AllowAgentForwarding ..."
	sudo sh -c 'echo "#nn Added by sshd.sh" >>/etc/ssh/sshd_config'
    sudo sh -c 'echo "AllowAgentForwarding yes" >>/etc/ssh/sshd_config'
fi
sudo systemctl restart ssh
