#!/usr/bin/env nu

let use_ssh = if ($env.USE_SSH? | is-not-empty) {
  not ($env.USE_SSH in ["false", "no", "n"])
} else {
  printf "Checking if github accessable through ssh"
  ^ssh -T git@github.com e>| str contains success
}
let editor = $env.EDITOR? | default "vim"
let difftool = $env.DIFF_TOOL? | default "kdiff3"

print $"use ssh: ($use_ssh)"
print $"editor: ($editor)"
print $"difftool: ($difftool)"

def negate [] { not $in }

def assert_fake_home [] {
  if (($env.FAKE_HOME | is-empty)
      or ($env.FAKE_HOME != $env.HOME)
      or ($env.FAKE_HOME == $env.OLD_HOME)
      or ($env.PWD != $env.FAKE_HOME)
     ) {
    error make {
      msg: $"This command wants FAKE_HOME. ($env.HOME) ($env.FAKE_HOME) ($env.PWD)"
    }
  }
}

def interactive_dialog [$header] {
  if ($in | is-empty) { return [] }

  let file = "dotfiles-interactive-dialog.txt"
  let marker = "--- *** Edit below this line *** ---"

  $header | lines | skip while { is-empty } | save -f $file
  $"\n($marker)\n\n" | save --append $file
  $in | save --append $file

  open-in-editor $file

  let res = open $file | lines | skip until { str contains $marker } | skip 1 | each { str trim } | where { is-not-empty }
  rm -f $file

  $res
}

def fake_home_env [] {
  let fake_home = "/tmp/fake-home.ba1f2511fc30423bdbb183fe33f3dd0f"
  { OLD_HOME: $env.HOME
  , FAKE_HOME: $fake_home
  , HOME: $fake_home
  }
}

def create_fake_home [] {
  let new_env = fake_home_env
  rm -rf $new_env.FAKE_HOME
  mkdir p $new_env.FAKE_HOME
  $new_env
}

def with-old-home [it] {
  with-env { HOME: $env.OLD_HOME } $it
}

def open-in-editor [file] {
  with-old-home {
    run-external $editor $file
  }
}

def with-new-fake-home [it] {
  with-env (create_fake_home) {
    cd $env.FAKE_HOME
    do $it
  }
}

def with-existing-fake-home [it] {
  with-env (fake_home_env) {
    cd $env.FAKE_HOME
    do $it
  }
}

def gh-url [] {
  let repo = $in
  if ($use_ssh) {
      $"git@github.com:($repo).git"
  } else {
      $"https://github.com/($repo)"
  }
}

def interactive_link_mr [] {
  assert_fake_home

  let options = do { cd .config/mr/available.d; ls } | get name
  let msg = $'mr tool manages groups of repositories
this configs has several groups defined, choose which ones to use:
'
  let res = $options | interactive_dialog $msg
  $res | each { |choice|
    ln -s -r -f $"($env.FAKE_HOME)/.config/mr/available.d/($choice)" $"($env.FAKE_HOME)/.config/mr/config.d/($choice)"
  }
  $res
}

# iterate over text files and replace references to FAKE_HOME with the
# original HOME value
def fix_vcsh_absolute_links [] {
  assert_fake_home
  cd ".config/vcsh/repo.d/"
  let $a = $env.FAKE_HOME
  let $b = $env.OLD_HOME
  ls -a **/* | where type == file | get name | each { |file|
    let is_text_file = ^file $file | str contains text
    if ($is_text_file) {
      let temp = mktemp
      open -r $file | each { str replace $a $b } | save -f -r $temp
      mv -f $temp $file
    }
  }
}

def clone_into_fake_home [] {
  assert_fake_home 

  vcsh clone ("kevroletin/dotfiles" | gh-url) dotfiles
  if (interactive_link_mr | is-not-empty) {
    mr update
  }

  fix_vcsh_absolute_links
}

def to_old_path [] {
  $"($env.OLD_HOME)/($in)"
}

def all_path_prefixes [] {
  if ($in | is-empty) {
    []
  } else {
    let xs = $in | path split
    0..($xs | length | $in - 1) | each { |idx| ($xs | slice 0..$idx) } | each { path join }
  }
}

def compute_file_conflicts [--ls-res: table] {
  assert_fake_home

  $ls_res
  | default (ls -a **/*)
  | where type in [file symlink]
  | get name
  | each { |file|
    let exists = $file | to_old_path | path exists
    { name: $file
    , is-conflict: (
        if ($exists) {
          (open -r $file) != (open -r ($file | to_old_path))
        } else {
          false
        }
      )
    , exists: $exists
    , type: file
    }
  }
}

def compute_dir_conflicts [--ls-res: table, --file-conflicts: table] {
  assert_fake_home

  let ls_res = $ls_res | default (ls -a **/*)
  let file_conflicts = $file_conflicts | default (compute_file_conflicts)

  let recursive_dir_conflicts = $file_conflicts
  | where is-conflict
  | get name
  | each { path dirname | all_path_prefixes }
  | flatten | each { [$in, true] } | into record

  $ls_res | where type == dir
  | get name
  | each { |dir|
    { name: $dir
    , is-conflict: ($recursive_dir_conflicts | get --optional $dir | default false)
    , exists: ($dir | to_old_path | path exists)
    , type: dir
    }
  }
}

def interactiv_resolve_conflicts [] {
  assert_fake_home

  let ls_res = ls -a **/*

  let repos = $ls_res
  | where type == dir
  | where { $in.name | path basename | $in == ".git" }
  | each { $in.name | path dirname }
  | append ".config/vcsh/repo.d/dotfiles.git"
  | sort | uniq

  let ignore_files = [
    "do.nu"
    , ".viminfo"
    , ".config/kdiff3rc"
    , "dotfiles-interactive-dialog.txt"
  ]
  let ignore_dirs = [
    , .cache
  ]
  let repos = do {
    let conflict_num = $repos | where { to_old_path | path exists } | length
    let num = $repos | length
    let msg = $"
There are ($num) git repos. ($conflict_num) of which already exist in the system.

We would ignore their content in dialogs and then just replace the whole
directory with new content.

You can edit this list:
"
    $repos | interactive_dialog $msg
  }

  # filter out ignored files
  let ls_res = do {
    let all_ignored_dirs = $ignore_dirs | append $repos
    def is_ok [] {
      let filename = $in
      let by_dir = $all_ignored_dirs | any { |ignored| 
        $filename | str starts-with $ignored
      }
      let by_file = $filename in $ignore_files
      not ($by_dir or $by_file)
    }
    $ls_res | where ($it.name | is_ok)
  }

  let files = compute_file_conflicts --ls-res=$ls_res

  let clean_files = do {
    let clean = $files 
    | where (not $it.is-conflict) and (not $it.exists)

    let msg = $"
There are ($clean | length) new files which have no conflicts.
We will copy these files as-is.
Please review/edit the list below:
"
    $clean | get name | interactive_dialog $msg
  }

  let conflict_files = do {
    let conflicts = $files | where is-conflict

    let $num = $conflicts | length
    let msg = $"
There are ($num) file conflicts, they are listed below.
Please edit the list to specify which files to copy.
In the next step we would propose to run diff for selected files:
"
    $conflicts | get name | interactive_dialog $msg
  }

  let diff_files = do {
    let layout = [ {"on the left": "existing system config", "on the right": "new config"}
                  , {"on the left": $env.OLD_HOME, "on the right": $env.HOME} ]
    let msg = $"
Difftool: ($difftool)

($layout | to md --pretty | into string)

Note: if you copy changes into a file on the right, these changes will be
copied into the system in the next steps.

Edit the list below to specify which files to diff:
"
    $conflict_files | interactive_dialog $msg
  }

  $diff_files | each {
    with-old-home {
      run-external $difftool ($in | to_old_path) ($in) | ignore 
    }
  }

  # generate resulting script
  let $res_script = do {
    let $res = "do.nu"
    let header = $'#!/usr/bin/env nu

cd "($env.FAKE_HOME)"

' | save -f $res

    $repos | each {
      let from = $in
      let to = ($in | to_old_path)

      [ $'try { rm -r "($to)" }'
      , $'cp -r "($from)" "($to)"']
    } | flatten | uniq | save --append $res

    "\n" | save --append $res

    $clean_files | each {
      let from = $in
      let to = ($in | to_old_path)

      [ $'mkdir "($to | path dirname)"'
      , $'cp "($from)" "($to)"']
    } | flatten | uniq | save --append $res

    "\n" | save --append $res

    $conflict_files | each {
      let from = $in
      let to = ($in | to_old_path)

      $'cp "($from)" "($to)"'
    } | save --append $res

    $res
  }

  open-in-editor $res_script
  print $"\n\nnu '($env.FAKE_HOME)/($res_script)'\n"

  let response = (input "Do you want to run the script? (y/n): ")
  if $response == "y" or $response == "Y" {
      nu $"($env.FAKE_HOME)/($res_script)"
  }
  print "Done"
}

def "main continue" [] {
  with-existing-fake-home {
    interactiv_resolve_conflicts
  }
  ignore
}

def "main clone" [] {
  with-new-fake-home {
    clone_into_fake_home
  }
  ignore
}

def "main install" [] {
  with-new-fake-home {
    clone_into_fake_home
    interactiv_resolve_conflicts
  }
  ignore
}

def main [] {
  "Execute one of the subcommands"
}
