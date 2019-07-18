#!/bin/bash

# Launch different websites


if [[ -n "$1" ]]; then
  open "$1"
  exit
fi

# see https://www.webfx.com/tools/emoji-cheat-sheet/
# echo ":wrench:"
echo "url"

echo "---"
echo "OneDrive | bash=$0 param1=https://mctools-my.sharepoint.com/personal/blezek_daniel_mayo_edu/_layouts/15/onedrive.aspx?id=%2Fpersonal%2Fblezek%5Fdaniel%5Fmayo%5Fedu%2FDocuments%2FRAIL%20Documents terminal=false"
echo "rail | bash=$0 param1=http://qia-jira.mayo.edu/rail/rail terminal=false"
# echo "rail-discuss | bash=$0 param1=http://qia-jira.mayo.edu/rail/rail-discuss/issues terminal=false"
echo "railtoolbox | bash=$0 param1=http://qia-jira.mayo.edu/rail/railtoolbox terminal=false"

echo "TFS | bash=$0 terminal=false param1=https://tfs.mayo.edu/tfs/MayoClinic/Radiology%20Operational%20Systems/Radiology%20AHI/_backlogs/taskboard?_a=requirements"

echo "---"
echo "trac | bash=$0 param1=http://ril-trac.mayo.edu/git-RIL terminal=false"
echo "trac tickets | bash=$0 param1=http://ril-trac.mayo.edu/git-RIL/report/10 terminal=false"

