

install:
#	git pull
	ln -sfn .dotfiles/emacs/.emacs ${HOME}
	ln -sfn .dotfiles/emacs/.emacs.d ${HOME}
	ln -sfn .dotfiles/bash/.bash_profile ${HOME}
	ln -sfn .dotfiles/bash/.bashrc ${HOME}
	ln -sfn .dotfiles/bash/.screenrc ${HOME}
	ln -sfn .dotfiles/slate/.slate ${HOME}
	ln -sfn .dotfiles/bash/.git-completion.bash ${HOME}
	ln -sfn ../.dotfiles/ssh/config ${HOME}/.ssh
	chmod 644 ${HOME}/.ssh/config

ssh:
	chmod 600 ${HOME}/.ssh/config

pull: ssh
	git pull


.PHONY: ssh pull install
