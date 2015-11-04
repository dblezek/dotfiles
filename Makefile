

install:
#	git pull
	ln -sfn .dotfiles/emacs/.emacs ${HOME}
	ln -sfn .dotfiles/emacs/.emacs.d ${HOME}
	ln -sfn .dotfiles/bash/.bash_profile ${HOME}
	ln -sfn .dotfiles/bash/.bashrc ${HOME}
	ln -sfn .dotfiles/slate/.slate ${HOME}
	ln -sfn ../.dotfiles/ssh/config ${HOME}/.ssh
