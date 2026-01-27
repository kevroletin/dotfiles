### TODO

symlinks

### How it works

The main problem of installing/updating dotfiles is that some files might
already exist. The installation script prevents unexpected losses like this:

1. Create a temporal "fake" home in /tmp directory
2. Checkout dotfiles and all the related git repositories in the fake home.
3. Compare the result file by file with the existing home and interactively
   resolve conflicts with user.
4. Generate a script to override existing files with the new ones and
   present is to the user before running.

Conflict resolution:
1. Override nested git repositories.

   For example ~/.xmonad/xmonad-contrib

2. Just copy new files and files without conflicts.
3. Run a difftool to merge existing files

### Installation

Configure github access

     scp ~/.ssh/* 192.168.1.104:~/.ssh/

Install Nushell and system tools

    # from https://www.nushell.sh/book/installation.html
    wget -qO- https://apt.fury.io/nushell/gpg.key | sudo gpg --dearmor -o /etc/apt/keyrings/fury-nushell.gpg
    echo "deb [signed-by=/etc/apt/keyrings/fury-nushell.gpg] https://apt.fury.io/nushell/ /" | sudo tee /etc/apt/sources.list.d/fury-nushell.list
    sudo apt update
    sudo apt install nushell

    sudo apt-get install curl git myrepos vcsh

Step 2: download and run script

     nu -e "
     curl https://raw.githubusercontent.com/kevroletin/dotfiles/master/.config/dotfiles/install.nu
       | save -f /tmp/dotfiles-install.nu
     nu /tmp/dotfiles-install.nu install
     "
