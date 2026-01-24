Installation
============

Step 1: configure github access

     scp ~/.ssh/* 192.168.1.104:~/.ssh/

Step 2: download and run script

     curl https://raw.githubusercontent.com/kevroletin/dotfiles/master/.config/dotfiles/install.nu > /tmp/install.nu
     sh /tmp/install.nu
