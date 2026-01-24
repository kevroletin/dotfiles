Installation
============

Warning: this script will delete old files and directories from your home.

Step 1: configure github access

     scp ~/.ssh/* 192.168.1.104:~/.ssh/

Step 2: download and run script

     curl https://raw.githubusercontent.com/kevroletin/dotfiles.mr/master/.dotfiles_bootstrap.d/install > /tmp/install
     sh /tmp/install
