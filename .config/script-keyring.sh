#!/bin/bash
set -e

echo "🔍 Checking pacman keyring directory..."
if [ -d /etc/pacman.d/gnupg ]; then
    ls -ld /etc/pacman.d/gnupg
    ls -l /etc/pacman.d/gnupg
else
    echo "❌ /etc/pacman.d/gnupg is missing!"
fi

echo
echo "🔍 Checking permissions..."
stat -c "%U:%G %a %n" /etc/pacman.d/gnupg

echo
echo "🔍 Checking gpg standalone import (test key)..."
if gpg --keyserver hkps://keys.openpgp.org --recv-keys 3056513887B78AEB; then
    echo "✅ gpg can fetch keys."
else
    echo "❌ gpg failed to fetch keys."
fi

echo
echo "🔍 Killing any stale dirmngr..."
sudo killall dirmngr 2>/dev/null || true

echo
echo "🔍 Testing pacman-key init..."
if sudo pacman-key -v --init; then
    echo "✅ pacman-key init succeeded."
else
    echo "❌ pacman-key init failed."
fi

echo
echo "🔍 Testing pacman-key populate archlinux..."
if sudo pacman-key -v --populate archlinux; then
    echo "✅ pacman-key populate archlinux succeeded."
else
    echo "❌ pacman-key populate archlinux failed."
fi

echo
echo "🔍 Finished diagnostics."
