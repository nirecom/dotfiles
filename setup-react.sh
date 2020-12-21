#!/bin/bash
# ref. https://qiita.com/TsutomuNakamura/items/72d8cf9f07a5a30be048
# Run at project directory
mkdir -p src/js
npm init
# Install Webpack packages
npm install --save-dev webpack webpack-cli webpack-dev-server
# Change owner of npm directories
# ref. https://kazuki19992.page/004_node_grobal_install_error/
sudo chown -R $(whoami) $(npm config get prefix)/{lib/node_modules,bin,share}
# Install webpack and webpack-cli
# On Ubuntu, got permission error without chown above.
npm install -g webpack webpack-cli
npm install @babel/core @babel/preset-env @babel/preset-react babel-loader
npm install --save-dev react react-dom
