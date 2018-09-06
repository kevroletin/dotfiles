#!exec sh

PACKAGES="vcsh mr zsh ranger ispell lynx vim"

echo "echo === Changing default shell to zsh ===" >> $RESULT_SCRIPT_FILE
echo "sudo chsh `whoami` -s `which zsh`" >> $RESULT_SCRIPT_FILE

while true; do
    read -p "Do you wish to install console version of emacs? " yn
    case $yn in
        [Yy]* ) PACKAGES="$PACKAGES emacs-nox"; break;;
        [Nn]* ) break;;
        * ) echo "Please answer yes or no.";;
    esac
done

echo "echo === Installing fzf using its own install script ===" >> $RESULT_SCRIPT_FILE
echo "~/.fzf/install" >> $RESULT_SCRIPT_FILE

echo $PACKAGES > $RESULT_PACKAGES_FILE
