#!/usr/bin/env sh

SRC="$(pwd)/home.nix"
TGT="$HOME/.config/home-manager/home.nix"

# Check if $TGT exists
if [ -e "$TGT" ]; then
    # Move existing $TGT to /tmp/
    mv "$TGT" /tmp/
    echo "Existing file $TGT moved to /tmp/"
fi

# Create symbolic link
ln -s "$SRC" "$TGT"
echo "Symbolic link created from $SRC to $TGT"
