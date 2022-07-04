# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

export LC_ALL=ja_JP.UTF-8
export EDITOR='code -w'

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/.local/bin" ] ; then
    PATH="$HOME/.local/bin:$PATH"
fi

export ANSIBLE_ENABLE_TASK_DEBUGGER=yes
export ANSIBLE_COLLECTIONS_PATHS=./.ansible/collections:~/.ansible/collections:/usr/share/ansible/collections
export ANSIBLE_ROLES_PATH=./.ansible/roles:~/.ansible/roles:/usr/share/ansible/roles:/etc/ansible/roles

gpgconf --launch gpg-agent
export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
