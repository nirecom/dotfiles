#!/bin/bash
# Initialize Ubuntu
source ./bin/detectos.sh

case "$OSDIST" in
    "amazon" )
        sudo yum update
        sudo yum install gcc
        cd
        wget https://jp.softether-download.com/files/softether/v4.41-9787-rtm-2023.03.14-tree/Linux/SoftEther_VPN_Server/64bit_-_ARM_64bit/softether-vpnserver-v4.41-9787-rtm-2023.03.14-linux-arm64-64bit.tar.gz
        tar zxvf softether-vpnserver-v4.41-9787-rtm-2023.03.14-linux-arm64-64bit.tar.gz
        cd vpnserver
        make
        cd ..
        sudo mv vpnserver /opt
        sudo chown root:root -R /opt/vpnserver
        sudo cat <<EOL >>/etc/systemd/system/vpnserver.service
[Unit]
Description=SoftEther VPN Server Service
After=network.target

[Service]
Type=forking
User=root
ExecStartPre=/sbin/ip link set dev eth0 promisc on
ExecStart=/opt/vpnserver/vpnserver start
ExecStop=/opt/vpnserver/vpnserver stop
Restart=on-abort
WorkingDirectory=/opt/vpnserver/

[Install]
WantedBy=multi-user.target
EOL
        sudo systemctl daemon-reload
        sudo systemctl start vpnserver
        sudo systemctl enable vpnserver
        ;;
    * )
        echo "This OS is not supported."

esac
