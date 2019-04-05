#!/bin/bash

# Launch different tmux sessions


if [[ -n "$1" ]]; then
  SCRIPTPATH="$( cd "$(dirname "$0")" ; pwd -P )"
  # Use applescript to launch a "rtmux" session
  osascript $SCRIPTPATH/../apple_scripts/rtmux.scpt "$1"
  exit
fi

# see https://www.webfx.com/tools/emoji-cheat-sheet/
# echo ":wrench:"
echo "rtmux"

echo "---"
echo "raildev1 | bash=$0 param1=raildev1 terminal=false"
echo "railgpu1 | bash=$0 param1=railgpu1 terminal=false"
echo "ril-gpu3 | bash=$0 param1=ril-gpu3 terminal=false"



