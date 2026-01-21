#
# Prezto
#

zstyle ':prezto:*:*' color 'yes'
# The order matters.
zstyle ':prezto:load' pmodule \
  'environment' \
  'terminal' \
  'editor' \
  'spectrum' \
  'utility' \
  'completion' \
  'prompt' \
  'git' \
  'history-substring-search' \
  'autosuggestions' \
  'syntax-highlighting' \
  'history'

  # history should be after autosuggestions otherwise it breaks auto suggestion
  # (test case: cd ~/.xmonad)
  # directory cause auto autosuggestion to flush after entering first character from
  # suggestion

zstyle ':prezto:module:editor' key-bindings 'emacs'
zstyle ':prezto:module:git:log:context' format 'oneline'
zstyle ':prezto:module:prompt' theme 'cloud' '$'
zstyle ':prezto:module:syntax-highlighting' highlighters \
  'main' \
  'brackets' \
  'pattern' \
  'line' \
  'root'
zstyle ':prezto:module:terminal' auto-title 'yes'

if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
    source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

#
# Manual config
#

alias xi="xclip -i -selection clipboard"
alias xo="xclip -o -selection clipboard"

alias kk=kubectl

# zsh-autosuggestions
#
# https://github.com/zsh-users/zsh-autosuggestions/blob/master/src/config.zsh
# forward-char doesn't accept entire suggestion
ZSH_AUTOSUGGEST_ACCEPT_WIDGETS=(end-of-line vi-end-of-line)
ZSH_AUTOSUGGEST_PARTIAL_ACCEPT_WIDGETS=(
    forward-char
    vi-forward-char
    forward-word
    vi-forward-word
    vi-forward-word-end
    vi-forward-blank-word
    vi-forward-blank-word-end
)

# zsh-history-substring-search
bindkey -M emacs '^P' history-substring-search-up
bindkey -M emacs '^N' history-substring-search-down
bindkey -M vicmd 'k' history-substring-search-up
bindkey -M vicmd 'j' history-substring-search-down

# ALT-X like in Emacs to list available commands
fzf-locate-widget() {
    local selected
    if selected=$(print -l ${(k)commands} ${(k)functions} ${(k)aliases} | fzf -q "$LBUFFER"); then
        LBUFFER=$selected
    fi
    zle redisplay
}
zle     -N    fzf-locate-widget
bindkey '\ex' fzf-locate-widget

integrate-clipboard() {
    # Integrate kill-buffer with X clipboard
    # https://gist.github.com/welldan97/5127861
    pb-kill-line () {
      zle kill-line
      echo -n $CUTBUFFER | xi
    }

    pb-kill-whole-line () {
      zle kill-whole-line
      echo -n $CUTBUFFER | xi
    }

    pb-backward-kill-word () {
      zle backward-kill-word
      echo -n $CUTBUFFER | xi
    }

    pb-kill-word () {
      zle kill-word
      echo -n $CUTBUFFER | xi
    }

    pb-kill-buffer () {
      zle kill-buffer
      echo -n $CUTBUFFER | xi
    }

    pb-copy-region-as-kill-deactivate-mark () {
      zle copy-region-as-kill
      zle set-mark-command -n -1
      echo -n $CUTBUFFER | xi
    }

    pb-yank () {
      CUTBUFFER=$(xo)
      zle yank
    }

    zle -N pb-kill-line
    zle -N pb-kill-whole-line
    zle -N pb-backward-kill-word
    zle -N pb-kill-word
    zle -N pb-kill-buffer
    zle -N pb-copy-region-as-kill-deactivate-mark
    zle -N pb-yank

    bindkey '^K'   pb-kill-line
    bindkey '^U'   pb-kill-whole-line
    bindkey '\e^?' pb-backward-kill-word
    bindkey '\e^H' pb-backward-kill-word
    bindkey '^W'   pb-backward-kill-word
    bindkey '\ed'  pb-kill-word
    bindkey '\eD'  pb-kill-word
    bindkey '^X^K' pb-kill-buffer
    bindkey '\ew'  pb-copy-region-as-kill-deactivate-mark
    bindkey '\eW'  pb-copy-region-as-kill-deactivate-mark
    bindkey '^Y'   pb-yank
}

if which xclip 2> /dev/null > /dev/null ; then
    integrate-clipboard
fi

# Use ranger to change current directory. Prevent recursive ranger.
#
# https://raw.githubusercontent.com/bitterjug/dotfiles/master/bash/rangercd.sh
# https://wiki.archlinux.org/index.php/ranger
ranger-cd() {
    if [ -z "$RANGER_LEVEL" ]
    then
        tempfile=$(mktemp)
        ranger --choosedir="$tempfile" "${@:-$(pwd)}" < $TTY
        test -f "$tempfile" &&
            if [ "$(cat -- "$tempfile")" != "$(echo -n `pwd`)" ]; then
                cd -- "$(cat "$tempfile")"
            fi
        rm -f -- "$tempfile"
    else
        exit
    fi
}

function yazi-cd() {
	  local tmp="$(mktemp -t "yazi-cwd.XXXXXX")" cwd
	  command yazi "$@" --cwd-file="$tmp"
	  IFS= read -r -d '' cwd < "$tmp"
	  [ -n "$cwd" ] && [ "$cwd" != "$PWD" ] && builtin cd -- "$cwd"
	  rm -f -- "$tmp"
}

# Bind Ctrl-O to ranger-cd. If ranger uses same key for entering shell then we
# will obtain consistent ranger-console switching.
zle -N yazi-cd
bindkey '^o' yazi-cd
# bindkey '^I' edit-command-line

# Press M-x to quickly find this function
_rand_password_stream() { < /dev/urandom tr -dc 'A-Z-a-z-0-9!"#$%&''()*+,-./:;<=>?@[\]^_{|}~,`' }

rand_password() { _rand_password_stream | head -c${1:-8}; echo }

rand_long_password() { _rand_password_stream | head -c${1:-16}; echo }

rand_password_clipboard() { _rand_password_stream | head -c${1:-8} | xi; xo; echo }

rand_long_password_clipboard() { _rand_password_stream | head -c${1:-16} | xi; xo; echo }

# https://stackoverflow.com/questions/27728838/using-hoogle-in-a-haskell-development-environment-on-nix?utm_medium=organic&utm_source=google_rich_qa&utm_campaign=google_rich_qa
env-type () {
  envtype="$1"
  shift
  nix-shell -Q -p $envtype "$@"
}

haskell-env () {
  env-type "haskellEnv" "$@"
}

haskell-env-hoogle () {
  env-type "haskellEnvHoogle" "$@"
}


copy-stack-tools() {
    stack build --copy-compiler-tool hlint hoogle weeder stylish-haskell hindent hasktags ghcid .
}

stackify() {
    compiler_bin=$(stack path --compiler-bin)
    compiler_tools_bin=$(stack path --compiler-tools-bin)
    if [ ! -d $compiler_tools_bin ]; then
        copy-stack-tools
    fi
    if [ ! -d $compiler_bin ]; then
        stack build --install-ghc
    fi
    PATH=$compiler_bin:$compiler_tools_bin:$PATH zsh
}

export KEYID=0x725E261B86255E90
export GPG_TTY=$(tty)

# kubectl package manager
export PATH="${KREW_ROOT:-$HOME/.krew}/bin:$PATH"

# pyenv
# --- slow startup ---
export PYENV_ROOT="$HOME/.pyenv"

export PATH="$PYENV_ROOT/bin:$PATH"
function pyenv-init () {
    eval "$(pyenv init --path)"
    eval "$(pyenv init -)"
    eval "$(pyenv virtualenv-init -)"
}

export NVM_DIR="$HOME/.nvm"
function nvm-init () {
    [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
    [ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
}

eval "$(zoxide init zsh)"

eval "$(direnv hook zsh)"

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
