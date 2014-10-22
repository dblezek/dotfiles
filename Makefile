

install:
	git pull
	stow emacs
	stow bash
	stow slate
	stow -t ~/.ssh ssh

remove:
	stow -vvD emacs
	stow -vvD bash
	stow -vvD slate
