module Packages (
  packages
) where

import           Software

packages :: [Software]
packages = concat [
    systemRelated
  , development
  , gcc
  , haskell
  , llvm
  , php
  , tex
  ]

systemRelated = [
    DnfPackages ["deltarpm", "drpm", "fakeroot"] none
  ]

gcc = [
    DnfPackages ["gcc", "make", "binutils"] none
  ]

llvm = [
    DnfPackages ["llvm", "clang"] none
  ]

development = [
    DnfPackages ["ctags", "gettext", "git", "gnome-terminal"] none
  ]

haskell = [

  ]

php = [
    DnfPackages ["php"] none
  ]

tex = [
    DnfPackages ["texlive-base", "biber"] none
  ]
