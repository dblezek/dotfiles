#!/bin/bash

# Launch different tmux sessions


if [[ -n "$1" ]]; then
  SCRIPTPATH="$( cd "$(dirname "$0")" ; pwd -P )"
    # ssh -t $1 "tmux -u -CC attach || tmux -u -CC"
  osascript $SCRIPTPATH/../apple_scripts/rtmux.scpt "$1"
  exit
fi

# see https://www.webfx.com/tools/emoji-cheat-sheet/
echo ":coffee:"

echo "---"
echo "raildev1 | bash=$0 param1=raildev1 terminal=false"
echo "railgpu1 | bash=$0 param1=railgpu1 terminal=false"
echo "ril-gpu3 | bash=$0 param1=ril-gpu3 terminal=false"

# osascript -e 'tell application "iTerm2" to create window with default profile command "/usr/local/bin/bash -l -c \"rtmux raildev1\""'


