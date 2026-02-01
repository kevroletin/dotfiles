use ../kv *

export def toggle_systemd_service [name] {
  if (^systemctl --user status $"($name).service" | str contains "Active: active") {
    systemctl --user stop $"($name).service"
  } else {
    systemctl --user start $"($name).service"
  }
}

export def --wrapped xmonadctl [...$rest] {
  run-external $"($env.HOME)/.local/bin/xmonadctl" ...$rest
}

export def --wrapped alacritty_tiled [...$rest --w=100 --h=30] {
  (^alacritty
    -o $"window.dimensions.columns=($w)" -o $"window.dimensions.lines=($h)"
    -e ...$rest)
}

export def --wrapped alacritty_float [...$rest --w=100 --h=30] {
  (^alacritty --class=xmonad-center-float
    -o $"window.dimensions.columns=($w)" -o $"window.dimensions.lines=($h)"
    -e ...$rest)
}

export def yazi_tiled [dir  --w=100 --h=30] {
  cd $dir
  alacritty_tiled yazi --w=$w --h=$h
}

export def yazi_float [dir  --w=100 --h=30] {
  cd $dir
  alacritty_float yazi --w=$w --h=$h
}

