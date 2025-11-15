#!/usr/bin/env bash

set -Eeuo pipefail

DOTFILES_DIR="$HOME/me/dotfiles"

relink() {
  local source="$1"
  local target="$2"

  if [ ! -e "$source" ]; then
    echo "Source file does not exist: $source"
    return 1
  fi

  local target_dir=$(dirname "$target")
  if [ ! -d "$target_dir" ]; then
    mkdir -p "$target_dir" || {
      echo "Failed to create directory: $target_dir"
      return 1
    }
    echo "Created directory: $target_dir"
  fi

  if [ -e "$target" ]; then
    echo "Skipped: $target (already exists)"
  else
    ln -sf "$source" "$target" || {
      echo "Failed to create symbolic link: $target"
      return 1
    }
    echo "Relinked: $source $target"
  fi
}

declare -A links=(
  ["wezterm"]="${HOME}/.config/wezterm"
  ["out/.tmux.conf"]="${HOME}/.tmux.conf"
  ["out/.zshrc"]="${HOME}/.zshrc"
  ["out/.zprofile"]="${HOME}/.zprofile"
  ["emacs/README.org"]="${HOME}/.emacs.d/README.org"
  ["emacs/init.el"]="${HOME}/.emacs.d/init.el"
  ["emacs/elfeed.el"]="${HOME}/.emacs.d/elfeed.el"
)

echo "Linking..."

for src in "${!links[@]}"; do
  relink "${DOTFILES_DIR}/${src}" "${links[$src]}"
done

echo "Done"
