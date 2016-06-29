#!/bin/sh

# The purpose of this file is to bootstrap `stack` and a Haskell environment
# so that the proper install script can do its work.

# Extract the Fedora version from a `uname --kernel-release` string like "4.4.13-200.fc22.x86_64"
function getFedoraVersion () {
  uname --kernel-release  | sed 's/.*fc\(..\).*/\1/'
}

# Verify that $USER is really the username
function verifyUsername () {
  read -p ">>> Is \`$USER\` your username? (y/n)" -r REPLY
  echo "" # Reset cursor
  if [[ $REPLY = "y" ]]; then # Test the positive case, in case the shell is broken
    return 0
  fi

  return 2
}

function bootstrapHaskell () {
  local USER=$1

  echo ">>> Adding \`$USER\` to wheel, enabling \`sudo\`"
  su -c "usermod \"$USER\" --append --groups wheel" ||
    { echo ">>> Could not add \`$USER\` to \`wheel\`" ; return 1; }

  echo ">>> Installing stack from \`fpcomplete\` repo"
  local FEDORA_VERSION=$(getFedoraVersion)
  local STACK_REPO="https://s3.amazonaws.com/download.fpcomplete.com/fedora/$FEDORA_VERSION/fpco.repo"
  curl -sSL $STACK_REPO | sudo tee /etc/yum.repos.d/fpco.repo

  sudo dnf -y install stack &&
    echo ">>> Successfully installed stack"
}

function callInstaller () {
  local INSTALL_SCRIPT=$1

  if [[ -f "$INSTALL_SCRIPT" && -r "$INSTALL_SCRIPT" ]]; then
    stack "$INSTALL_SCRIPT"
  else
    echo ">>> Installer script \"$INSTALL_SCRIPT\" could not be found"
    return 2
  fi
}

(verifyUsername && bootstrapHaskell "$USER" && callInstaller Installer.hs) || exit 1
