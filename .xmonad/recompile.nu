#!/usr/bin/env nu

stack build

open dependencies.txt | lines | each { |dep|
  stack install $"($dep | str trim)"
}

xmonad --recompile
