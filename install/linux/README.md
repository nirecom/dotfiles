# Install repository

Automated installation scripts of OS configuration, necessary tools (e.g. awscli), home directory setup, and language / frameworks.

* How it works

    * These files are intended to place under ~/install.
    * On cleaned install OS, install initially with sequential init scripts (os-init.sh, nire-init.sh). They invoke small scripts to setup each packages / tools.
    * Or, you can run each script independently.

* System Requirement
    * OS: Either of
        * Ubuntu 20.04 LTS (native Ubuntu or WSL2)
        * macOS
        * Git for Windows
    * Shell
        * bash or zsh

* Install
    * cd ~
    * git clone git@github.com:nirecom/install.git
    * cd install
    * You need to change $USERNAME to your local account name.
    * ./os-init.sh
    * login with user account
    * ./home-init.sh

* Cleanup obsolete files
    * ./home-obsolete.sh
        * This cleans files that are not used now.
