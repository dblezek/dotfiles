install: brew
#	git pull
	ln -sfn .dotfiles/emacs/.emacs ${HOME}
	ln -sfn .dotfiles/emacs/.emacs.d ${HOME}
	ln -sfn .dotfiles/bash/.bash_profile ${HOME}
	ln -sfn .dotfiles/bash/.bashrc ${HOME}
	ln -sfn .dotfiles/bash/.curl-format ${HOME}
	ln -sfn .dotfiles/bash/.screenrc ${HOME}
	ln -sfn .dotfiles/bash/.ctags ${HOME}
	ln -sfn .dotfiles/slate/.slate ${HOME}
	ln -sfn .dotfiles/bash/.git-completion.bash ${HOME}
	ln -sfn .dotfiles/bash/.z.sh ${HOME}
	ln -sfn .dotfiles/bash/.iterm2_shell_integration.bash ${HOME}
	ln -sfn .dotfiles/bash/.isiterm.sh ${HOME}
	ln -sfn ../.dotfiles/ssh/config ${HOME}/.ssh
	chmod 644 ${HOME}/.ssh/config

ssh:
	chmod 600 ${HOME}/.ssh/config

pull: ssh
	git pull

# Dump brew so we can rebuild using brew bundle
HN:=$(shell hostname -s)
BREW := $(shell command -v brew 2> /dev/null )
brew:
ifdef BREW
	brew bundle dump --file=${HN}.brew --force
endif


.PHONY: ssh pull install
