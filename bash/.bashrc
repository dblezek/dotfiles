# Source global definitions

# touching ~/.hushlogin is very useful for suppressing /etc/motd
case "$-" in
    *i*) INTERACTIVE=1 ;;
    *) INTERACTIVE=0 ;;
esac

if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# A fair number of settings were taken from https://github.com/mrzool/bash-sensible/blob/master/sensible.bash

# MacPorts Installer addition on 2009-02-11_at_16:17:44: adding an appropriate PATH variable for use with MacPorts.
export PATH=/opt/local/bin:/opt/local/sbin:$PATH
export MANPATH=/opt/local/share/man:$MANPATH

# Catch any locally installed files
export PATH=/usr/local/bin:$PATH

# Test directory
export MI3CTESTDATADIR=${HOME}/Source/MI3CTestData
export MI3CTESTOUTPUTDIR=/tmp/

# Local packages
export PATH=${HOME}/Source/bin:$PATH

alias CC="emacs -nw CMakeCache.txt"
alias rm="rm -i"
alias cp="cp -i"
alias mv="mv -i"
alias ls="ls -GFh"
alias lsl="ls -GF -lah"
alias dir=ls
alias top="top -u"
alias g="./gradlew"

# curl-trace (https://github.com/wickett/curl-trace)
alias curl-trace='curl -w "@$HOME/.curl-format"'

function enscriptCode() {  enscript --line-numbers -numbers -Ecpp -r2 -o - "$@" | ps2pdf - "$@".pdf; }

# Prevent file overwrite on stdout redirection
set -o noclobber

# Update window size after every command
shopt -s checkwinsize

# History functions
# Append to the history file, don't overwrite it
shopt -s histappend

# Save multi-line commands as one command
shopt -s cmdhist

# Record each line as it gets issued
# PROMPT_COMMAND='history -a'

# Huge history. Doesn't appear to slow things down, so why not?
HISTSIZE=500000
HISTFILESIZE=100000

# Avoid duplicate entries
HISTCONTROL="erasedups:ignoreboth"

# Don't record some commands
export HISTIGNORE="&:[ ]*:exit:ls:bg:fg:history:clear"

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

# Maven?
if [ -e $HOME/Source/maven ]; then
    export PATH=${PATH}:$HOME/Source/maven/bin
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
export DYLD_LIBRARY_PATH=${HOME}/.macosx/lib:$DYLD_LIBRARY_PATH
export PATH=${HOME}/.macosx/bin:${HOME}/.macosx/dcm4che/bin:$PATH

# Add developer tools
# export PATH=$PATH:/Developer/Platforms/iPhoneOS.platform/Developer/usr/bin
# instead (cd ${HOME}/.macosx/bin && ln -s /Developer/Platforms/iPhoneOS.platform/Developer/usr/bin/texturetool .)
# We only really want texturetool...

# Editor
export EDITOR=emacs

# git completion commands
source $HOME/.git-completion.bash

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


# Color the GIT branch
# PS1='\h:\W\[\e[1;34m\]$(parse_git_branch)\[\e[0m\] \u\$ '

# No colors
PS1='\h:\W$(parse_git_branch) \u\$ '

PS1=$'\[\e]2;\h:\]$PWD\[\a\]\[\e]1;\]$(basename "$(dirname "$PWD")")/\W\[\a\][\@]{\u:\h}\W\#: '
PS1=$'\[\e]2;\h:\]$PWD\[\a\]\[\e]1;\]$(basename "$(dirname "$PWD")")/\W\[\a\][\@]{\u:\h$(parse_git_branch)}:\W\n\#: '

if [[ `hostname -s` = myst ]]; then
    PS1='\h:\W$(parse_git_branch) \u: '
fi

if [[ $TERM = dumb ]]; then
    PS1=':$'
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
    env -i MAILTO=blezek.daniel@mayo.edu SHELL=/bin/sh USER=$USER PATH=/usr/bin:/bin PWD=/home/noop SHLVL=1 LOGNAME=$LOGNAME _=/usr/bin/env HOME=$HOME /bin/bash --noprofile --norc -c "$t"
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

# Locally installed python
if [ -d $HOME/Library/Python/2.7/bin/ ]; then
    export PATH=$HOME/Library/Python/2.7/bin:$PATH
fi

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

# RCF settings
if [ -f /home/oge/ge2011.11/default/common/settings.sh ]; then
  . /home/oge/ge2011.11/default/common/settings.sh
fi

# Source RCF Cluster Definitions
if [ -f $HOME/.bash_mayobiotools ]; then
  if [ $INTERACTIVE = '1' ]; then
	. $HOME/.bash_mayobiotools
  else
	. $HOME/.bash_mayobiotools > /dev/null
  fi
fi

# UDP settings
if [[ `hostname -s` = hdpr03en01 ]]; then
    # emacs keybindings
    set -o emacs
    export PATH=$PATH:/data/home/aau/bin
fi    
umask 0002

# added by Anaconda3 4.1.0 installer
if [ -f $HOME/anaconda/bin/python ]; then
    export PATH="/Users/mra9161/anaconda/bin:$PATH"
fi

