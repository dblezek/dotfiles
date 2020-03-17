#!/bin/bash

# Mount sshfs filesystems


if [[ -n "$1" ]]; then
  P=$1
  DIR=$2
  # sshfs raildev1:/home/mra9161 ~/raildev1
  # to debug
  # echo /usr/local/bin/sshfs -odebug,sshfs_debug,loglevel=debug raildev1.mayo.edu:$P $HOME/sshfs/$DIR
  mkdir -p "$HOME/sshfs/$DIR"
  echo /usr/local/bin/sshfs -o volname=$DIR "raildev1.mayo.edu:$P" "$HOME/sshfs/$DIR"
  /usr/local/bin/sshfs -o volname=$DIR "raildev1.mayo.edu:$P" "$HOME/sshfs/$DIR"
  exit
fi

# see https://www.webfx.com/tools/emoji-cheat-sheet/
# echo ":wrench:"
echo "sshfs"

echo "---"
echo "home | bash=$0 param1=/home/mra9161 param2=raildev1-home terminal=false"
echo "root | bash=$0 param1=/ param2=raildev1-root terminal=false"
echo "scratch | bash=$0 param1=/scratch/djb param2=raildev1-scratch terminal=true"
echo "project | bash=$0 param1=/project param2=raildev1-project terminal=false"



