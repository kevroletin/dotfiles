use ../kv *

export def modify-opacity [f] {
  let config = $"($env.HOME)/.config/alacritty/alacritty.toml"
  let template = $"($config).template"
  let opacity = open -r $config | parse "opacity = {x}" | first | get x | default 1.0 | into float
  let res = (do $f ($opacity * 100 | into int)) / 100
  nu $"($env.HOME)/.xmonad/expand-template" --file $template --vars ({opacity: $res} | to nuon)
}

export def toggle-opacity [] {
  modify-opacity { |current|
    let old = $current | store_key "alacritty.opacity" | default 100
    if ($current >= 100) {
      if ($old < 100) { $old } else { 85 }
    } else {
      100
    }
  }
}

export def more-opacity [] { modify-opacity { |x| if ($x >= 95) { 100 } else { $x + 05 } } }
export def less-opacity [] { modify-opacity { |x| if ($x <= 5) { 0 } else { $x - 5 } } }
export def set-opacity [val] { modify-opacity { |_x| $val } }
export def less-brightness [] { run-external $"($env.HOME)/bin/adjust-brightness" "-" }
export def more-brightness [] { run-external $"($env.HOME)/bin/adjust-brightness" "+" }
export def knob_mode [mode] { nu $"($env.HOME)/.xmonad/knob-action" set-mode $mode }
export def --wrapped theme-switcher [...$rest] { nu $"($env.HOME)/.xmonad/theme-switcher" ...$rest }
