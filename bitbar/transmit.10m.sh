#!/bin/bash

# Launch different Transmit sessions


if [[ -n "$1" ]]; then
  SCRIPTPATH="$( cd "$(dirname "$0")" ; pwd -P )"
  osascript $SCRIPTPATH/../apple_scripts/OpenTransmitServer.scpt "$1"
  exit
fi

# see https://www.webfx.com/tools/emoji-cheat-sheet/
echo "transmit"

echo "---"
echo "railgpu1 | bash=$0 param1=railgpu1 terminal=false"
echo "raildev1 | bash=$0 param1=raildev1 terminal=false"
echo "fod1 | bash=$0 param1=fod1 terminal=false"
echo "phone | bash=$0 param1=phone terminal=false"
echo "rcf | bash=$0 param1=rcf-ftp terminal=false"

# osascript -e 'tell application "iTerm2" to create window with default profile command "/usr/local/bin/bash -l -c \"rtmux raildev1\""'


