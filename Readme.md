# dotfiles

A simple place to share all my emacs bindings and customizations.

Inspiration form http://taihen.org/managing-dotfiles-with-gnu-stow/ and http://brandon.invergo.net/news/2012-05-26-using-gnu-stow-to-manage-your-dotfiles.html?round=two

# Setup

```
cat /dev/zero | ssh-keygen -b 4096 -C "daniel.blezek@gmail.com $(hostname)" -q -N ""
```

Copy `~/.ssh/id_rsa.pub` up to [github](https://github.com/settings/keys), [bitbucket](https://bitbucket.org/account/user/blezek/ssh-keys/), etc.... 

```
git clone git@github.com:blezek/dotfiles.git .dotfiles
(cd .dotfiles && make)
. .bashrc
# aah, good to be home
```

## Bash 4 on Mac

1. Install brew: `/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"`
2. install bash: `brew install bash`
3. let the Mac use the new bash: `sudo bash -c 'echo /usr/local/bin/bash >> /etc/shells'`
4. change my shell to bash 4: `chsh -s /usr/local/bin/bash`

## Fonts

```sh
# Source Code Pro
brew tap caskroom/fonts && brew cask install font-source-code-pro
# Input font
https://input.fontbureau.com/
```

# Git configuration

Tell `git` to put log lines in merge messages, and setup username and email.

``` sh
git config --global --add merge.log true
git config --global user.name "Daniel Blezek"
git config --global user.email daniel.blezek@gmail.com
```



