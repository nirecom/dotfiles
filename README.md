# dotfiles

Dot* files / directories (e.g. .bash_profile, .emacs.d) under the home directory.

* How it works

These files are intended to place under ~/dotfiles. Necessary files are linked to their expected ~/ location by setup shell script (dotfileslink.sh).

* System Requirement

It automatically detects OS and changes behaviors.

    * OS: Either of
	    * Ubuntu 20.04 LTS (native Ubuntu or WSL2)
		* macOS
		* Git for Windows
	* Shell
	    * bash or zsh

* Install

    * Install "install" repository firstly to install dependent packages.
	See https://github.com/nirecom/install
    * cd ~
	* git clone git@github.com:nirecom/dotfiles.git
	* cd dotfiles
	* ./dotfileslink.sh
	
Thanks,
Hideaki Nire (@nirecom)
