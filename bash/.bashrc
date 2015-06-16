
# MacPorts Installer addition on 2009-02-11_at_16:17:44: adding an appropriate PATH variable for use with MacPorts.
export PATH=/opt/local/bin:/opt/local/sbin:$PATH
# Finished adapting your PATH environment variable for use with MacPorts.

# Catch any locally installed files
export PATH=/usr/local/bin:$PATH

# MI3CLib path
export PATH=${HOME}/Source/MI3CLib-macosx/bin:$PATH
export PATH=${HOME}/.macosx/play/:$PATH
export PATH=${HOME}/.macosx/BladeRunnerJS/sdk/:$PATH

# MacPorts Installer addition on 2009-02-11_at_16:17:44: adding an appropriate MANPATH variable for use with MacPorts.
export MANPATH=/opt/local/share/man:$MANPATH
# Finished adapting your MANPATH environment variable for use with MacPorts.

# DCMTK dictionary, not needed as of 2010-04-14
# export DCMDICTPATH=/opt/local/lib/dicom.dic

# Test directory
export MI3CTESTDATADIR=${HOME}/Source/MI3CTestData
export MI3CTESTOUTPUTDIR=/tmp/

# Local packages
export PATH=${HOME}/Source/bin:$PATH

alias CC="emacs -nw CMakeCache.txt"
alias rm="rm -i"
alias cp="cp -i"
alias mv="mv -i"
alias ls="ls -GF"
alias lsl="ls -GF -la"
alias dir=ls
alias top="top -u"
alias iMI3C-debug="${HOME}/Source/MI3CLib-macosx-debug/bin/iMI3C"
alias QMon="ssh -f -X mi3c qmon"
alias g="./gradlew"
alias atom="/Applications/Atom.app/Contents/MacOS/Atom"

function enscriptCode() {  enscript --line-numbers -numbers -Ecpp -r2 -o - "$@" | ps2pdf - "$@".pdf; }

# ignore duplicates
export HISTCONTROL=erasedups

# Java, choose the most recent
export JAVA_HOME=`/usr/libexec/java_home -v 1.7+`

# Use logout to exit the shell
set -o ignoreeof

set show-all-if-ambiguous on
set visible-stats on


# For linux3JCB7L
# export LS_COLORS='no=00:fi=00:di=01;36:ln=01;36:pi=40;33:so=01;35:bd=40;33;01:cd=40;33;01:or=01;05;37;41:mi=01;05;37;41:ex=01;32:*.cmd=01;32:*.exe=01;32:*.com=01;32:*.btm=01;32:*.bat=01;32:*.sh=01;32:*.csh=01;32:*.tar=01;31:*.tgz=01;31:*.arj=01;31:*.taz=01;31:*.lzh=01;31:*.zip=01;31:*.z=01;31:*.Z=01;31:*.gz=01;31:*.bz2=01;31:*.bz=01;31:*.tz=01;31:*.rpm=01;31:*.cpio=01;31:*.jpg=01;35:*.gif=01;35:*.bmp=01;35:*.xbm=01;35:*.xpm=01;35:*.png=01;35:*.tif=01;35:'


# make directories grey...
export LSCOLORS='gxfxcxdxbxegedabagacad'


# VMTK
# export PATH=${HOME}/Source/bin:$PATH  # Already added
# export DYLD_LIBRARY_PATH=${HOME}/.macosx/bin:${HOME}/Source/VTK-macosx/bin:$DYLD_LIBRARY_PATH
# export PYTHONPATH=${HOME}/.macosx/lib/vmtk/:${HOME}/Source/VTK-macosx/Wrapping/Python/:${HOME}/Source/VTK-macosx/bin:$PYTHONPATH
# source /Applications/vmtk.app/Contents/MacOS/vmtk

export PYTHONPATH=${HOME}/.macosx/python/lib/python2.7/site-packages:$PYTHONPATH
# Change the title of the terminal window
function titleold() { echo -ne "\e]2;$@\a\e]1;$@\a"; }

# function for setting terminal titles in OSX
function title {
  printf "\033]0;%s\007" "$1"
}

# installed software
export DYLD_LIBRARY_PATH=${HOME}/.macosx/lib:$DYLD_LIBRARY_PATH
export PATH=${HOME}/.macosx/bin:${HOME}/.macosx/dcm4che/bin:$PATH
export PATH=${HOME}/Source/MI3CLib/Applications/PipelineApps:$PATH

# Add developer tools
# export PATH=$PATH:/Developer/Platforms/iPhoneOS.platform/Developer/usr/bin
# instead (cd ${HOME}/.macosx/bin && ln -s /Developer/Platforms/iPhoneOS.platform/Developer/usr/bin/texturetool .)
# We only really want texturetool...

##
# Your previous ${HOME}/.profile file was backed up as ${HOME}/.profile.macports-saved_2010-04-01_at_15:44:49
##

# MacPorts Installer addition on 2010-04-01_at_15:44:49: adding an appropriate PATH variable for use with MacPorts.
export PATH=/opt/local/bin:/opt/local/sbin:$PATH
# Finished adapting your PATH environment variable for use with MacPorts.

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

# Make our terminal names more helpful to Timing
PROMPT_TITLE='echo -ne "\033]0;${USER}@${HOSTNAME%%.*}:${PWD/#$HOME/~}\007"'
export PROMPT_COMMAND="${PROMPT_COMMAND} ${PROMPT_TITLE}; "

# Correcting directory names
shopt -s cdspell

# CCACHE prefix is to use distcc
export CCACHE_PREFIX="distcc"


function cronenv () {
    # Expand it here, then quote it on the next line
    t=$@
    env -i MAILTO=blezek.daniel@mayo.edu SHELL=/bin/sh USER=$USER PATH=/usr/bin:/bin PWD=/home/noop SHLVL=1 LOGNAME=$LOGNAME _=/usr/bin/env HOME=$HOME /bin/bash --noprofile --norc -c "$t"
}

# Helper for SimpleITK stage
# Outmoded by SimpleITK developer script
# alias stage="ssh git@itk.org stage SimpleITK"

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
export PATH=${HOME}/Source/go/bin:${HOME}/Source/go-bin/bin:${PATH}

# GO
export GOPATH=${HOME}/Source/go-bin:${HOME}/Source/corsair:${HOME}/Source/DEWEY/kiln:${HOME}/Source/bumped:${HOME}/Source/grunt:${HOME}/Source/grunt/vendor

function ec () {
    osascript -e 'tell application "Emacs" to activate' && /Applications/Emacs.app/Contents/MacOS/bin/emacsclient --no-wait "$@"
}
