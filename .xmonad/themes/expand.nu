#!/usr/bin/env nu
#
# find files names xxx.template
# if xxx is a symlink, write to it's destination
# otherwise write xxx
#

def expand [pallete] {
  str replace -r -a '\(\$[\w\.\-]+\)' {
    let key = $in | parse "(${res})" | get res | first
    print $"Searching for ($key)"
    $pallete | get $key
  }
}

def find_templates [] {
  ls -al *
  | where ($it.type == file) and ($it.name | str ends-with ".template")
  | get name
  | each { |template|
    let bn = $template | parse "{x}.template" | get x | first
    let target = try {
      ls -l $bn | first | get target | default $bn
    } catch {
      $bn
    }

    { from: $template
    , to: $target
    }
  }
}

def main [--dir=".", --vars:string] {
  let pallete = open $vars
  cd $dir

  print "Vars:"
  print $pallete
  find_templates | each { |t|
    print $t
    open $t.from | expand $pallete | save -f $t.to
  }
  ignore
}
