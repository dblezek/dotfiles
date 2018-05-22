# Source global definitions

# touching ~/.hushlogin is very useful for suppressing /etc/motd
case "$-" in
  *i*) INTERACTIVE=1 ;;
  *) INTERACTIVE=0 ;;
esac

if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# /etc/profile.d sets some toxic variables...
unset ITK_DIR
unset VTK_DIR

# A fair number of settings were taken from https://github.com/mrzool/bash-sensible/blob/master/sensible.bash

# MacPorts Installer addition on 2009-02-11_at_16:17:44: adding an appropriate PATH variable for use with MacPorts.
# export PATH=/opt/local/bin:/opt/local/sbin:$PATH
# export MANPATH=/opt/local/share/man:$MANPATH

# Catch any locally installed files
export PATH=/usr/local/bin:$PATH

# Local packages
# export PATH=${HOME}/Source/bin:$PATH

alias CC="emacs -nw CMakeCache.txt"
# Copy last command into the clipboard (Mac) http://apple.stackexchange.com/questions/110343/copy-last-command-in-terminal
alias cc="history 2 | head -1 | cut -d ' ' -f 4- | pbcopy"
alias rm="rm -i"
alias cp="cp -i"
alias mv="mv -i"
alias ls="ls -GFh"
alias lsl="ls -GF -lah"
alias la="lsl"
alias lr="ls -lhtr"
alias dir=ls
# alias top="top -u"
alias g="./gradlew"
alias l="less"
alias tags="ctags -e -R"
# Activate a Python virtulenv
# alias activate="source */bin/activate"
# curl-trace (https://github.com/wickett/curl-trace)
alias curl-trace='curl -w "@$HOME/.curl-format"'
# always show everyone's jobs in the SGE
alias qstat="qstat -u '*' -f"

alias ccat="ccat --color=always"
alias cgrep="grep --color=always"
alias ag='\ag --pager=less'
alias httpd='python -m SimpleHTTPServer'
# Quick nav
alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias .....="cd ../../../.."

# Parallel
alias parallel="$HOME/.dotfiles/bash/parallel"

# Get a website recursively
# https://www.guyrutenberg.com/2014/05/02/make-offline-mirror-of-a-site-using-wget/
alias WGET="wget -mkEpnp"

# Mac specific things
ARCH=$(uname)
if [[ "$ARCH" == "Darwin" ]]; then
  alias emacs="/Applications/Emacs.app/Contents/MacOS/Emacs -nw --no-desktop"
  export DYLD_LIBRARY_PATH=${HOME}/.macosx/lib:/opt/X11/lib/flat_namespace:$DYLD_LIBRARY_PATH
  export PATH=${HOME}/.macosx/bin:$PATH
  export PATH=${PATH}:/Applications/VMware\ OVF\ Tool/
  export PATH=${HOME}/.macosx/node_modules/.bin/:${PATH}

  # Renderman
  export RMANTREE=/Applications/Pixar/RenderManProServer-21.3
  export PATH=${RMANTREE}/bin:${RMANTREE}/bin/it.app/Contents/MacOS/it:${PATH}
  alias top="top -o cpu"
fi

if [[ "$ARCH" == "Linux" ]]; then
  export PATH=${HOME}/.software/bin:${PATH}
  if [ -e /opt/pixar/RenderManProServer-21.4/ ]; then
    # Renderman
    export RMANTREE=/opt/pixar/RenderManProServer-21.4/
    export PATH=${PATH}:${RMANTREE}/bin
  fi
  # Use color in Linux
  alias ls="ls -Fh --color"
fi


function enscriptCode() {  enscript --line-numbers -numbers -Ecpp -r2 -o - "$@" | ps2pdf - "$@".pdf; }

# Allow file overwrite on stdout redirection
set +o noclobber

# Update window size after every command
shopt -s checkwinsize

# History functions
# Append to the history file, don't overwrite it
shopt -s histappend

# Save multi-line commands as one command
shopt -s cmdhist

# Use the physical directory structure instead of symbolic links
# http://stackoverflow.com/questions/10456784/behavior-of-cd-bash-on-symbolic-links
set -o physical

# Record each line as it gets issued
# PROMPT_COMMAND='history -a'

# Huge history. Doesn't appear to slow things down, so why not?
HISTSIZE=500000
HISTFILESIZE=100000

# Avoid duplicate entries
HISTCONTROL="erasedups:ignoreboth"

# Don't record some commands
# Specifically, ignore any command that has a leading space using the ' *' pattern
export HISTIGNORE="&:[ ]*:exit:ls:bg:fg:history:clear: *"

# Useful timestamp format
# HISTTIMEFORMAT='%F %T '

# Have PROMPT_COMMAND rewrite history to include the directory
# Unused for the moment...
# export PROMPT_COMMAND='hpwd=$(history 1); hpwd="${hpwd# *[0-9]*  }"; if [[ ${hpwd%% *} == "cd" ]]; then cwd=$OLDPWD; else cwd=$PWD; fi; hpwd="${hpwd% ### *} ### cd $cwd"; history -s "$hpwd"'

# Have PROMPT_COMMAND log every command to a log file
# https://spin.atomicobject.com/2016/05/28/log-bash-history/
mkdir -p $HOME/.bash-history-log
export PROMPT_COMMAND='if [ "$(id -u)" -ne 0 ]; then echo "$(date "+%Y-%m-%d.%H:%M:%S") $(pwd) $(history 1)" >> ~/.bash-history-log/bash-history-$(date "+%Y-%m-%d").log; fi'

# Make our terminal names more helpful to Timing
PROMPT_TITLE='echo -ne "\033]0;${USER}@${HOSTNAME%%.*}:${PWD/#$HOME/~}\007"'
export PROMPT_COMMAND="${PROMPT_COMMAND}; ${PROMPT_TITLE}; "


# Java, choose the most recent
if [ -e /usr/libexec/java_home ]; then
  export JAVA_HOME=`/usr/libexec/java_home -v 1.8+`
else
  export JAVA_HOME=/usr/java/latest/
fi

# Use logout to exit the shell
set -o ignoreeof

# Make sure we use emacs
set -o emacs

## SMARTER TAB-COMPLETION (Readline bindings) ##

# Perform file completion in a case insensitive fashion
bind "set completion-ignore-case on"

# Treat hyphens and underscores as equivalent
bind "set completion-map-case on"

# Display matches for ambiguous patterns at first tab press
bind "set show-all-if-ambiguous on"

bind "set visible-stats on"

# LESS options
export LESS="-XF --search-skip-screen --ignore-case --raw-control-chars"

# make directories grey...
export LSCOLORS='gxfxcxdxbxegedabagacad'

# Change the title of the terminal window
function titleold() { echo -ne "\e]2;$@\a\e]1;$@\a"; }

# function for setting terminal titles in OSX
function title {
  printf "\033]0;%s\007" "$1"
}

# installed software

# Add developer tools
# export PATH=$PATH:/Developer/Platforms/iPhoneOS.platform/Developer/usr/bin
# instead (cd ${HOME}/.macosx/bin && ln -s /Developer/Platforms/iPhoneOS.platform/Developer/usr/bin/texturetool .)
# We only really want texturetool...

# Editor
export EDITOR=emacs

# git completion commands
source $HOME/.git-completion.bash
alias gs='git status'
alias gc='git commit -m' # requires you to type a commit message
alias gp='git push'
alias gl='git pull'
alias gf='git fetch'
alias git-cleanup-branches='git branch --merged | egrep -v "(^\*|master|dev)" | xargs -n 1 git branch -d'

# Help out git a bit
# alias cmerge='git cmerge'
# complete -o default -o nospace -F _git_checkout cmerge

# Git branch on the prompt
function parse_git_branch {
  # This version has ()'s
  git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\1)/'
}
function current_git_branch {
  # no ()'s
  git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/\1/'
}

# Alias to help when a branch has moved on
alias gup='git fetch origin && git rebase -p origin/$(git_current_branch)'


# Detect TRAMP from emacs and play dumb
if [[ $TERM = dumb ]]; then
  PS1='> '
else
  # iTerm2
  # from https://gitlab.com/gnachman/iterm2/issues/4743
  # Next line gives all sorts of headaches... like C-c exits the shell?!?, so skip the ".isiterm.sh"
  # source ${HOME}/.isiterm.sh && source $HOME/.iterm2_shell_integration.bash
  # if [ -z ${COLORTERM+x} ]; then source $HOME/.iterm2_shell_integration.bash; fi
  [[ $(uname) == "Darwin" ]] && source $HOME/.iterm2_shell_integration.bash
  # Color the GIT branch
  # PS1='\h:\W\[\e[1;34m\]$(parse_git_branch)\[\e[0m\] \u\$ '

  # No colors
  PS1='\h:\W$(parse_git_branch) \u\$ '
  
  PS1=$'\[\e]2;\h:\]$PWD\[\a\]\[\e]1;\]$(basename "$(dirname "$PWD")")/\W\[\a\][\@]{\u:\h}\W\#: '
  PS1=$'\[\e]2;\h:\]$PWD\[\a\]\[\e]1;\]$(basename "$(dirname "$PWD")")/\W\[\a\]\[\@\]{\u:\h$(parse_git_branch)}:\W\n\#: '
  
  PS1=$'\[\e]2;\h:\]$PWD\[\a\]\[\e]1;\]$(basename "$(dirname "$PWD")")/\W\[\a\]\@{\u:\h$(parse_git_branch)}:\W\n\#: '
  
  if [[ `hostname -s` = myst ]]; then
    PS1='\h:\W$(parse_git_branch) \u: '
  fi
fi

# cd options
# Correcting directory names
shopt -s cdspell
# Prepend cd to directory names automatically, silence where not available
if shopt | grep autocd > /dev/null; then
  shopt -s autocd
fi
# Correct spelling errors during tab-completion, silenced
if shopt | grep dirspell > /dev/null; then
  shopt -s dirspell
fi

# This allows you to bookmark your favorite places across the file system
# Define a variable containing a path and you will be able to cd into it regardless of the directory you're in
shopt -s cdable_vars

export dotfiles="$HOME/.dotfiles"
export Source="$HOME/Source"
export dropbox="$HOME/Dropbox"


# Setup a gradle properties file
if [ ! -f $HOME/.gradle/gradle.properties ]; then
  mkdir -p $HOME/.gradle
  touch ~/.gradle/gradle.properties && echo "org.gradle.daemon=true" >> ~/.gradle/gradle.properties
fi

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

# Clean up history by ignoring certain items
export HISTIGNORE="&:ls"
export HISTCONTROL=erasedups

# bash-completion
if [ -f /opt/local/etc/bash_completion ]; then
  . /opt/local/etc/bash_completion
fi

# Ignore files in completion, dumb .DS_Store files!
export FIGNORE=.DS_Store

# Node modules
export PATH=./node_modules/.bin:$PATH

# Editor for commit messages
export EDITOR="emacs -nw"

# Correcting directory names
shopt -s cdspell

# Use logout to exit the shell
set -o ignoreeof

set show-all-if-ambiguous on
set visible-stats on

# NPM, see .npmrc for details
export PATH=${HOME}/.macosx/npm/bin:${PATH}
export PATH=${HOME}/.macosx/bin:${PATH}

# Bump up open files
ulimit -n 2048

# Cross-compiling GO, and standard go-bin
# export PATH=${HOME}/Source/go/bin:${HOME}/Source/go-bin/bin:/usr/local/go/bin:${PATH}
export PATH=/usr/local/go/bin:${HOME}/Source/go/bin:${PATH}
export GO15VENDOREXPERIMENT=1

# GO
# "go get" by default installs in the first directory, make sure it's in our path.
export GOPATH=${HOME}/Source/go

function ec () {
  osascript -e 'tell application "Emacs" to activate' && /Applications/Emacs.app/Contents/MacOS/bin/emacsclient --no-wait "$@"
}


# Source RCF Cluster Definitions
if [ -f $HOME/.bash_mayobiotools ]; then
  if [ $INTERACTIVE = '1' ]; then
	  . $HOME/.bash_mayobiotools
          # RCF settings
          if [ -f /home/oge/ge2011.11/default/common/settings.sh ]; then
              . /home/oge/ge2011.11/default/common/settings.sh
          fi
  else
	  . $HOME/.bash_mayobiotools > /dev/null
  fi
fi

# Look for Freesurfer
for fsdir in "${HOME}/Applications/freesurfer" /Applications/freesurfer; do
  if [ -e "$fsdir" ]; then
    export FREESURFER_HOME="$fsdir"
    source "$FREESURFER_HOME/SetUpFreeSurfer.sh" > /dev/null 2>&1 
  fi
done

# Look for FSL
for fsldir in "${HOME}/Applications/fsl" /usr/local/fsl; do
  if [ -e "$fsldir" ]; then
    export FSLDIR="$fsldir"
    source "${FSLDIR}/etc/fslconf/fsl.sh" > /dev/null 2>&1
    export PATH=${FSLDIR}/bin:${PATH}
  fi
done

# Helper to manage all the paths
function add_path() {
  if [ -d "$1" ]; then
    export PATH=${PATH}:"$1"
    return 1
  fi
  return 0
}

# add_path $HOME/anaconda/bin
# add_path /research/projects/DJB/anaconda/bin
add_path $HOME/Applications/MRIcron
add_path $HOME/Applications/node_modules/.bin
add_path $HOME/Applications/mrtrix/bin
add_path $HOME/Source/ntr.plugins/build/mrtrix3/bin
# pip local executables
add_path $HOME/.local/bin
# catch itksnap
add_path $HOME/Applications
# catch itksnap
add_path /Applications/Convert3DGUI.app/Contents/bin

# Matlab?
add_path /Applications/MATLAB_R2016b.app/bin/
# MacTex 2016 under El Capitan
add_path /Library/TeX/texbin
# AFNI
add_path $HOME/Applications/afni

# FSLEyes
add_path $HOME/Applications/FSLeyes.app/Contents/MacOS

# Maven?
add_path $HOME/Source/maven/bin
# brew
add_path /usr/local/sbin
# Blender
add_path /Applications/blender/blender.app/Contents/MacOS

# HCP Workbench
add_path $HOME/Applications/workbench/bin_macosx64

# RCF tools
add_path /data5/radiology/bje01/mra9161/mricron_lx
add_path /data5/radiology/bje01/mra9161/mrtrix3/release/bin
# add_path /data5/radiology/bje01/shared/anaconda/bin

# Remote tmux connection
function rtmux {
  ssh -t $1 "tmux -u -CC attach || tmux -u -CC"
}

# Silently set the session name
# Check if TMUX is set and tmux exists and then rename the session to the hostname, if it's not already set
[[ -v TMUX ]] && [[ -n "$TMUX" ]] && command -v tmux >/dev/null 2>&1 && [[ $(tmux display-message -p '#S') == $(hostname) || $(tmux rename-session $(hostname)) ]]

# history grep
function hgrep {
  if hash ag 2>/dev/null; then
    # Use ag, if it exists
    ag $1 ${HOME}/.bash-history-log/
  else
    # Fall back to good old grep
    grep -i $1 ${HOME}/.bash-history-log/*
  fi
}

# z from https://github.com/rupa/z
if [ -e ${HOME}/.z.sh ]; then
  . ${HOME}/.z.sh
fi

# Detect TRAMP from emacs and play dumb
if [[ $TERM = dumb ]]; then
  PS1='> '
else
  # iTerm2
  # from https://gitlab.com/gnachman/iterm2/issues/4743
  # Next line gives all sorts of headaches... like C-c exits the shell?!?, so skip the ".isiterm.sh"
  # source ${HOME}/.isiterm.sh && source $HOME/.iterm2_shell_integration.bash
  # if [ -z ${COLORTERM+x} ]; then source $HOME/.iterm2_shell_integration.bash; fi
  source $HOME/.iterm2_shell_integration.bash
  # Color the GIT branch
  # PS1='\h:\W\[\e[1;34m\]$(parse_git_branch)\[\e[0m\] \u\$ '

  # No colors
  PS1='\h:\W$(parse_git_branch) \u\$ '
  
  PS1=$'\[\e]2;\h:\]$PWD\[\a\]\[\e]1;\]$(basename "$(dirname "$PWD")")/\W\[\a\][\@]{\u:\h}\W\#: '
  PS1=$'\[\e]2;\h:\]$PWD\[\a\]\[\e]1;\]$(basename "$(dirname "$PWD")")/\W\[\a\]\[\@\]{\u:\h$(parse_git_branch)}:\W\n\#: '
  
  PS1=$'\[\e]2;\h:\]$PWD\[\a\]\[\e]1;\]$(basename "$(dirname "$PWD")")/\W\[\a\]\@{\u:\h$(parse_git_branch)}:\W\n\#: '
  
  if [[ `hostname -s` = myst ]]; then
    PS1='\h:\W$(parse_git_branch) \u: '
  fi
fi

