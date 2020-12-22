#!/bin/bash
# Install stable node version via n package
# ref. https://qiita.com/seibe/items/36cef7df85fe2cefa3ea
# ref. https://qiita.com/kiwi-bird/items/e3e551938d09282cf4ee
sudo apt update
sudo apt install -y nodejs npm
sudo npm -g install n
sudo n stable
# remove older nodejs, npm
sudo apt purge -y nodejs npm
# relogin shell
exec $SHELL -l
# check version
node -v
