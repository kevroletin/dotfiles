
use std/log *

export-env {
    $env.STORAGE_DIR = $"($env.HOME)/.local/share/dotfiles/kv"
}

export def store_keys [] {
  if not ($env.STORAGE_DIR | path exists) {
    mkdir $env.STORAGE_DIR
  }
  $in | items { |k v|
    let $file = $"($env.STORAGE_DIR)/($k).nuon"

    let $old_v = if ($file | path exists) {
      try { open $file } catch { null }
    } else {
      null
    }

    # info $"Writing ($k) ($v)"
    $v | to nuon | save -f $file
    { ($k): $old_v }
  } | into record
}

export def store_key [k] {
  {($k): $in} | store_keys | get $k
}

export def load_keys [] {
  $in | each { |k|
    let $file = $"($env.STORAGE_DIR)/($k).nuon"

    let $v = if ($file | path exists) {
      open $file
    } else {
      null
    }

    # info $"Loaded ($k) ($v)"
    [$k $v]
  } | into record
}

export def load_key [] {
  let $k = $in
  [$k] | load_keys | get $k
}
