if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

if [ -d $HOME/.local/bin ]; then
    PATH=$HOME/.local/bin:$PATH
fi

if [ -d $HOME/.profile.d ]; then
    for i in $HOME/.profile.d/*.sh; do
        if [ -r $i ]; then
            . $i
        fi
    done
    unset i
fi

export SHELL=/usr/bin/zsh
export EDITOR=/usr/bin/vim
export LC_ALL="en_US.UTF-8"
export LANGUAGE="en_US.UTF-8"

. "$HOME/.cargo/env"
