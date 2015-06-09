# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

export PS1="[\u@\H] \w\n\$ "

source ~/.aliases

# User specific aliases and functions
