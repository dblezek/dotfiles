# Source global definitions

# touching ~/.hushlogin is very useful for suppressing /etc/motd
case "$-" in
  *i*) INTERACTIVE=1 ;;
  *) INTERACTIVE=0 ;;
esac

if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# Detect TRAMP from emacs and play dumb
if [[ $TERM == "dumb" ]]; then
  PS1='> '
  return
fi

# A fair number of settings were taken from https://github.com/mrzool/bash-sensible/blob/master/sensible.bash
. $HOME/.dotfiles/bash/.bash_functions

# Catch any locally installed files
export PATH=/usr/local/bin:$PATH

# Specific for GNU Tar on Mac
export PATH="/usr/local/opt/gnu-tar/libexec/gnubin:$PATH"

alias CC="emacs -q -nw CMakeCache.txt"
# Copy last command into the clipboard (Mac) http://apple.stackexchange.com/questions/110343/copy-last-command-in-terminal
alias cc="history 2 | head -1 | cut -d ' ' -f 4- | pbcopy"
alias rm="rm -i"
alias cp="cp -i"
alias mv="mv -i"
alias ls="ls -GFh"
alias lsl="ls -GF -lah"
alias la="lsl"
alias lr="ls -lhtr"
alias lrs="ls -lhSr"
alias g="./gradlew"
alias l="less"
alias tags="ctags -e -R"
# curl-trace (https://github.com/wickett/curl-trace)
alias curl-trace='curl -w "@$HOME/.curl-format"'
alias tfversion="python3 -c 'import tensorflow as tf; print(tf.__version__)'"

alias ccat="ccat --color=always"
alias cgrep="grep --color=always"
alias httpd='python -m http.server'
alias blue='blueutil -p 0; sleep 3s; blueutil -p 1'

# some helpers
alias e="emacs -q -nw"
alias pc="pbcopy"
alias pp="pbpaste"

# Quick nav
alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias .....="cd ../../../.."
alias fv='freeview \
      -v mri/T1.mgz \
      -v mri/aseg.mgz:colormap=lut:opacity=0.2 \
      -f surf/lh.white:edgecolor=yellow \
      -f surf/rh.white:edgecolor=yellow \
      -f surf/lh.pial:annot=aparc:edgecolor=red \
      -f surf/rh.pial:annot=aparc:edgecolor=red \
      -f surf/lh.thickness \
      -f surf/rh.thickness '

# see https://community.atlassian.com/t5/Bitbucket-questions/can-t-commit/qaq-p/719732
# for how to use the a GUI as needed
alias gpg-reset='export GPG_TTY=$(tty) ; gpgconf --kill gpg-agent'


export SQUEUE_FORMAT="%.22i %.4P %.18j %.8u %.12M  %R"
if hash srun 2>/dev/null; then
  alias SRUN="srun --job-name=rcnn --dependency=singleton --partition=m32 --mem-per-cpu=10G --gres=gpu:1 "
fi

# if pbcopy does not exist, use the Perl script from
# https://github.com/skaji/remote-pbcopy-iterm2
if ! hash hstr 2>/dev/null; then
  alias pbcopy=$HOME/.dotfiles/bash/pbcopy
fi

# Parallel
alias parallel="$HOME/.dotfiles/bash/parallel"

# Get a website recursively
# https://www.guyrutenberg.com/2014/05/02/make-offline-mirror-of-a-site-using-wget/
alias WGET="wget -mkEpnp"

# Mac specific things
ARCH=$(uname)
if [[ "$ARCH" == "Darwin" ]]; then
  # Simple shell script for emacs
  if [[ ! -e $HOME/Applications/bin/emacs ]]; then
    mkdir -p $HOME/Applications/bin/
    ln -sfn $HOME/.dotfiles/bash/emacs $HOME/Applications/bin/
    chmod 755 $HOME/Applications/bin/emacs
  fi  

  # Renderman
  export RMANTREE=/Applications/Pixar/RenderManProServer-22.6
  export PATH=${RMANTREE}/bin:${RMANTREE}/bin/it.app/Contents/MacOS/it:${PATH}
  alias top="top -o cpu"
fi

if [[ "$ARCH" == "Linux" ]]; then
  export PATH=${HOME}/.software/bin:${PATH}

  if [[ -e "/opt/pixar" ]]; then
    # Guess at Renderman version
    r=$(find /opt/pixar/ -maxdepth 1 -type d -name 'RenderManProServer-*' | sort --reverse | head -n 1 )
    if [ -e ${r} ]; then
      # Renderman
      export RMANTREE=$r
      export PATH=${PATH}:${RMANTREE}/bin
    fi
  fi
  # Use color in Linux
  alias ls="ls -Fh --color"
fi

# confirm history expansions
if shopt | grep histverify > /dev/null; then
  shopt -s histverify
fi

# Allow file overwrite on stdout redirection
set +o noclobber

# Update window size after every command
shopt -s checkwinsize

# History functions
# Append to the history file, don't overwrite it
shopt -s histappend

# Save multi-line commands as one command
# shopt -s cmdhist

# Use the physical directory structure instead of symbolic links
# http://stackoverflow.com/questions/10456784/behavior-of-cd-bash-on-symbolic-links
set -o physical

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

# Don't be fancy expanding '$', just substitute them
shopt -s direxpand

# Huge history. Doesn't appear to slow things down, so why not?
export HISTSIZE=500000
export HISTFILESIZE=100000

# show date and time in history
HISTTIMEFORMAT="%D %r "

# Don't record some commands
# Specifically, ignore any command that has a leading space using the ' *' pattern
# taking out the space because cut/paste from emacs gets broken...
# export HISTIGNORE="&:[ ]*:cd:exit:ls:bg:fg:rm:history:clear"
export HISTIGNORE="cd:exit:ls:bg:fg:rm:history:clear"

# Have PROMPT_COMMAND log every command to a log file
# https://spin.atomicobject.com/2016/05/28/log-bash-history/
mkdir -p $HOME/.bash-history-log
export PROMPT_COMMAND='if [ "$(id -u)" -ne 0 ]; then echo "$(date "+%Y-%m-%d-%H:%M") $(pwd) $(history 1 | cut -c 8-)" >> ~/.bash-history-log/bash-history-$(date "+%Y-%m-%d").log; fi'

# Java, choose the most recent
if [[ -z ${JAVA_HOME+x} ]]; then
   if [[ -e /usr/libexec/java_home ]]; then
     export JAVA_HOME=`/usr/libexec/java_home -v 1.8+`
   elif [[ -e /usr/java/latest ]]; then
     export JAVA_HOME=/usr/java/latest/
   fi
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
export LESS="--search-skip-screen --ignore-case -R -X "

# control less colors make directories grey...
export LSCOLORS='gxfxcxdxbxegedabagacad'

# installed software

# cheat from https://github.com/cheat/cheat
source $HOME/.dotfiles/cheat/.cheat-completion.bash
export CHEAT_CONFIG_PATH="$HOME/.dotfiles/cheat/config.yml"


# git completion commands
source $HOME/.git-completion.bash
alias git-cleanup-branches='git branch --merged | egrep -v "(^\*|master|dev)" | xargs -n 1 git branch -d'
alias git-tree='git log --oneline --graph --color --decorate --all'

# Setup a gradle properties file
if [ ! -f $HOME/.gradle/gradle.properties ]; then
  mkdir -p $HOME/.gradle
  touch ~/.gradle/gradle.properties && echo "org.gradle.daemon=true" >> ~/.gradle/gradle.properties
fi

# bash-completion
if [ -f /opt/local/etc/bash_completion ]; then
  . /opt/local/etc/bash_completion
fi

# Ignore files in completion, dumb .DS_Store files!
export FIGNORE=.DS_Store

# Editor for commit messages
export EDITOR="emacs -nw -q"

# Bump up open files
ulimit -n 2048

# GO
# Cross-compiling GO, and standard go-bin
export PATH=/usr/local/go/bin:${HOME}/Source/go/bin:${PATH}
# "go get" by default installs in the first directory, make sure it's in our path.
export GOPATH=${HOME}/Source/go


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
for fsdir in "${HOME}/Applications/freesurfer" /Applications/freesurfer /home/apps/freesurfer/latest/; do
  if [ -e "$fsdir" ]; then
    export FREESURFER_HOME="$fsdir"
    source "$FREESURFER_HOME/SetUpFreeSurfer.sh" > /dev/null 2>&1
    if [[ "$ARCH" == "Darwin" ]]; then
      export DYLD_LIBRARY_PATH=$FREESURFER_HOME/lib/gcc/lib
    fi
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

add_path $HOME/Applications/bin
add_path $HOME/Applications/mrtrix/bin
add_path /home/apps/mrtrix3/bin
add_path $HOME/Source/mrtrix3/bin
add_path $HOME/Source/railway-build/bin

# pip local executables
add_path $HOME/.local/bin
# catch itksnap
add_path $HOME/Applications
# catch itksnap
add_path /Applications/Convert3DGUI.app/Contents/bin

# MacTex 2016 under El Capitan
add_path /Library/TeX/texbin
# AFNI
add_path $HOME/Applications/afni

# FSLEyes
add_path $HOME/Applications/FSLeyes.app/Contents/MacOS

# brew
add_path /usr/local/sbin

# Blender
add_path /Applications/blender/blender.app/Contents/MacOS

# HCP Workbench
add_path $HOME/Applications/workbench/bin_macosx64

# GCP SDK
add_path $HOME/Applications/google-cloud-sdk/bin/

# ~/Applications/bin
add_path $HOME/Applications/bin



# See https://github.com/dvorka/hstr/blob/master/CONFIGURATION.md
# to configure hstr
if hash hstr 2>/dev/null; then
    if [[ $- =~ .*i.* ]]; then
        # search this shell's history only!
        # send the 
        bind '"\C-r": "\C-a history -w /tmp/hh.$$ && HISTFILE=/tmp/hh.$$ hstr -- \C-j"';
        alias hh=hstr
        export HSTR_CONFIG=hicolor,prompt-top,raw-history-view
    fi
fi


# See if we have the ag command
if hash ag 2>/dev/null; then
  alias ag='\ag --pager=less'
else
  alias ag='grep --recursive '
fi

# Autocomplete
# See
# https://debian-administration.org/article/317/An_introduction_to_bash_completion_part_2
# https://unix.stackexchange.com/a/181603/261626
function _ssh() 
{
    local cur prev opts
    COMPREPLY=()
    cur="${COMP_WORDS[COMP_CWORD]}"
    prev="${COMP_WORDS[COMP_CWORD-1]}"
    opts=$(grep '^Host' ~/.ssh/config ~/.ssh/config.d/* 2>/dev/null | grep -v '[?*]' | cut -d ' ' -f 2-)

    COMPREPLY=( $(compgen -W "$opts" -- ${cur}) )
    return 0
}
complete -F _ssh ssh
complete -F _ssh rtmux

# from https://gitlab.com/gnachman/iterm2/issues/4743
export ITERM_ENABLE_SHELL_INTEGRATION_WITH_TMUX=yes
source $HOME/.iterm2_shell_integration.bash

# No colors
# PS1='\h:\W$(parse_git_branch) \u\$ '
# PS1=$'\[\e]2;\h:\]$PWD\[\a\]\[\e]1;\]$(basename "$(dirname "$PWD")")/\W\[\a\][\@]{\u:\h}\W\#: '
# PS1=$'\[\e]2;\h:\]$PWD\[\a\]\[\e]1;\]$(basename "$(dirname "$PWD")")/\W\[\a\]\[\@\]{\u:\h$(parse_git_branch)}:\W\n\#: '
PS1=$'\[\e]2;\h:\]$PWD\[\a\]\[\e]1;\]$(basename "$(dirname "$PWD")")/\W\[\a\]\@{\u:\h$(parse_git_branch)}:\W\n\#: '

# Homebrew setup
if type brew &>/dev/null; then
  HOMEBREW_PREFIX=$(brew --prefix)
  if [[ -r "${HOMEBREW_PREFIX}/etc/profile.d/bash_completion.sh" ]]; then
    source "${HOMEBREW_PREFIX}/etc/profile.d/bash_completion.sh"
  else
    for COMPLETION in "${HOMEBREW_PREFIX}/etc/bash_completion.d/"*; do
      [[ -r "$COMPLETION" ]] && source "$COMPLETION"
    done
  fi
fi


# Any local customization?
if [ -f $HOME/.bashrc_local ]; then
    . $HOME/.bashrc_local
fi



