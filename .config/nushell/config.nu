# config.nu
#
# Installed by:
# version = "0.109.1"
#
# This file is used to override default Nushell settings, define
# (or import) custom commands, or run any other startup tasks.
# See https://www.nushell.sh/book/configuration.html
#
# Nushell sets "sensible defaults" for most configuration settings, 
# so your `config.nu` only needs to override these defaults if desired.
#
# You can open this file in your default editor using:
#     config nu
#
# You can also pretty-print and page through the documentation for configuration
# options using:
#     config nu --doc | nu-highlight | less -R
$env.config.buffer_editor = "nvim"
$env.config.show_banner = false
$env.config.use_kitty_protocol = true

def --env y [...args] {
  let tmp = (mktemp -t "yazi-cwd.XXXXXX")
  ^yazi ...$args --cwd-file $tmp
  let cwd = (open $tmp)
  if $cwd != $env.PWD and ($cwd | path exists) {
    cd $cwd
  }
  rm -fp $tmp
}

$env.config.keybindings ++= [
  {
    name: change_dir_with_yazi
    modifier: CONTROL
    keycode: Char_o
    mode: [emacs, vi_normal, vi_insert]
    event: {
        send: executehostcommand,
        cmd: "y"
    }
  },
  {
    name: open_editor
    modifier: CONTROL
    keycode: Char_i
    mode: [emacs, vi_normal, vi_insert]
    event: { send: openeditor }
  }
]

source .zoxide.nu
