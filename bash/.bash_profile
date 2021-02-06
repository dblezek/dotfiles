

[[ -r ~/.bashrc ]] && . ~/.bashrc


# MacPorts Installer addition on 2016-01-11_at_12:25:21: adding an appropriate PATH variable for use with MacPorts.
# export PATH="/opt/local/bin:/opt/local/sbin:$PATH"



#test -e "${HOME}/.iterm2_shell_integration.bash" && source "${HOME}/.iterm2_shell_integration.bash"


# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/Users/mra9161/Applications/anaconda3/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/Users/mra9161/Applications/anaconda3/etc/profile.d/conda.sh" ]; then
        . "/Users/mra9161/Applications/anaconda3/etc/profile.d/conda.sh"
    else
        export PATH="/Users/mra9161/Applications/anaconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<

