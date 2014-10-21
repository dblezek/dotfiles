

install:
	git pull
	stow emacs
	stow bash
	stow slate

remove:
	stow -vvD emacs
	stow -vvD bash
	stow -vvD slate
