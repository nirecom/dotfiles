#!/bin/bash
source ./bin/detectos.sh

if [ "$OSDIST" = "mingw" ] || [ "$OSDIST" = "macos" ] || [ -n "${ISWSL:-}" ]; then
    echo "Not necessary to configure /etc/sshd_config because it's client. Abort."
    exit 1
fi

# Allow Agentforwarding to SSH
sudo grep -q -E '^ *AllowAgentForwarding' /etc/ssh/sshd_config
if [ $? -gt 0 ]; then
    echo "Adding AllowAgentForwarding ..."
    sudo sh -c 'echo "#nn Added by sshd.sh" >>/etc/ssh/sshd_config'
    sudo sh -c 'echo "AllowAgentForwarding yes" >>/etc/ssh/sshd_config'
fi
echo "Restarting sshd"
case "$OSDIST" in
    "ubuntu" ) sudo systemctl restart ssh ;;
    "amazon" ) 
        echo "Amazon Linux..."
        if grep "Amazon Linux 2" /etc/system-release >/dev/null 2>&1; then
            echo "Detected Amazon linux 2..."
            sudo systemctl restart ssh
        else
            sudo service sshd restart
        fi
        ;;
esac
