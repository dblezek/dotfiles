
function tfgpus {
  python3 -c 'import tensorflow as tf; print(tf.config.experimental.list_physical_devices("GPU"))'
}

# List SSH hosts
function hosts {
  grep -v "#" ${HOME}/.ssh/config | grep "Host "  | awk '{$1=""; print $0}' 
}

function dl () {
  scp -r $* R5174775:./Downloads/
  # rsync --relative -r $* R5174775:./Downloads/
}

# make a directory and cd into it, never used
function md () {
  mkdir -p "$1"
  cd "$1"
}

function dll () {
  temp=$(mktemp)
  for f in $*; do
    printf "rsync -r --info=progress2" >> $temp
    printf " $(hostname):$(realpath $f) .\n" >> $temp
  done
  cat $temp | $HOME/.dotfiles/bash/pbcopy
  rm -f $temp
}

# Shortcut to get the real path and copy to clipboard
function rp {
  realpath "$@" | pbcopy
}

# like ag, but for files
function ff() {
  find . -iname "*${1}*"
}


function cronenv () {
  # Expand it here, then quote it on the next line
  t=$@
  env -i \
      MAILTO=blezek.daniel@mayo.edu \
      SHELL=/bin/sh \
      USER=$USER \
      PATH=/usr/bin:/bin \
      PWD=$PWD \
      SHLVL=1 \
      LOGNAME=$LOGNAME \
      _=/usr/bin/env \
      HOME=$HOME /bin/sh --noprofile --norc -c "$t"
}

# Change iTerm tab name
function tname() {
  # do nothing if outside of TMUX
  if [ ! -z $TMUX ] ; then
   tmux rename-window "$@"
  fi
}

function ec () {
  osascript -e 'tell application "Emacs" to activate' && /Applications/Emacs.app/Contents/MacOS/bin/emacsclient --no-wait "$@"
}

# Helper to manage all the paths
function add_path() {
  if [ -d "$1" ]; then
    export PATH=${PATH}:"$1"
    return 1
  fi
  return 0
}

# Remote tmux connection
function rtmux {
  ssh -t $1 "tmux -u -CC attach || tmux -u -CC"
}

# history grep
function hgrep {
    if hash ag 2>/dev/null; then
      find ${HOME}/.bash-history-log/ -type f | sort | xargs ag "$1"
    else
      # Fall back to good old grep
      find ${HOME}/.bash-history-log/ -type f | sort | xargs grep -i "$1"
    fi
  # fi
}

# mirror up to a remote server
function mirror {
  uname=$(uname)
  tmp=$(mktemp)
  for f in "$@"; do
    p=$(realpath $f)
    p=${p/Users/home}

    
    if [[ $uname == "Linux" ]]; then
      printf 'rsync -arv "raildev1:%s" "%s"\n' "$(realpath $f)" "$p" >> $tmp
    else
      # printf 'rsync -arv "%s" "%s"\n' "$(realpath $f)" "$p"
      printf "upload %s to %s\n" "$f" "raildev1:$p"
      rsync -ar "$(realpath $f)" "raildev1:$p"
    fi
  done
  if [[ $uname == "Linux" ]]; then
    cat "$tmp"
    rm -f "$tmp"
  fi
}

function dl {
  tmp=$(mktemp)
  for f in "$@"; do
    p=$(realpath $f)
    p=${p/Users/home}
    printf 'rsync -arv "raildev1:%s" "%s"\n' "$(realpath $f)" "." >> $tmp
  done
    cat "$tmp"
    rm -f "$tmp"
}

# Git branch on the prompt
function parse_git_branch {
  # This version has ()'s
  git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\1)/'
}
function current_git_branch {
  # no ()'s
  git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/\1/'
}


