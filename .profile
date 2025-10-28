# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

if locale -a | grep -i ja_JP.utf8 &>/dev/null; then
    export LC_ALL=ja_JP.utf8
fi
if which code &>/dev/null; then
    export EDITOR='code -w'
fi

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

# set PATH so it includes project's private bin
PATH=".venv/bin:$PATH"

# Remove trailing slash in XDG_RUNTIME_DIR (for WSL)
export XDG_RUNTIME_DIR="${XDG_RUNTIME_DIR%/}"

if [ -d "$HOME/.fly" ] ; then
    export FLYCTL_INSTALL="$HOME/.fly"
    export PATH="$FLYCTL_INSTALL/bin:$PATH"
fi

# if running on WSL
if which wslpath &>/dev/null; then
    export WIN_USER="$(powershell.exe '$env:USERNAME' | sed -e 's/\r//g')"
    export WIN_HOME="$(wslpath "$(powershell.exe '$env:USERPROFILE' | sed -e 's/\r//g')")"
    if [ -d "/mnt/c/Program Files/AutoHotkey" ] ; then
        AHK_DIR_PATH="/mnt/c/Program Files/AutoHotkey"
    fi
    if [ -d "/mnt/c/Program Files (x86)/AutoHotkey" ] ; then
        AHK_DIR_PATH="/mnt/c/Program Files (x86)/AutoHotkey"
    fi
    if [ -d "$HOME/OneDrive/Apps/AutoHotkey" ] ; then
        AHK_DIR_PATH="$HOME/OneDrive/Apps/AutoHotkey"
    fi
    PATH="$AHK_DIR_PATH:$PATH"
fi

export ANSIBLE_ENABLE_TASK_DEBUGGER=yes
export ANSIBLE_COLLECTIONS_PATH=./.ansible/collections:~/.ansible/collections:/usr/share/ansible/collections
export ANSIBLE_ROLES_PATH=./.ansible/roles:~/.ansible/roles:/usr/share/ansible/roles:/etc/ansible/roles

export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
