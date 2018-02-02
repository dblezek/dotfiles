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



# Git configuration

Tell `git` to put log lines in merge messages.

``` sh
git config --global --add merge.log true
```
