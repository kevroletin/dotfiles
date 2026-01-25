Installation
============

Configure github access

     scp ~/.ssh/* 192.168.1.104:~/.ssh/

System tools

     sudo apt-get update
     sudo apt-get install curl git myrepos vcsh

     # TODO: install nushell

Step 2: download and run script

     nu -e "
     curl https://raw.githubusercontent.com/kevroletin/dotfiles/master/.config/dotfiles/install.nu
       | save -f /tmp/dotfiles-install.nu
     nu /tmp/dotfiles-install.nu install
     "


