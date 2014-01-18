# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

# if running bash
if [ -n "$BASH_VERSION" ] && [ -n "$TERM" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ]; then
    PATH="$HOME/bin:$PATH"
fi

if [ -d "$HOME/.local/bin" ]; then
    PATH="$HOME/.local/bin:$PATH"
fi

if [ -z "$SSH_AUTH_SOCK" ] && which ssh-agent > /dev/null; then
    eval $(ssh-agent)
fi

if [ -e "$HOME/.pythonrc.py" ]; then
    export PYTHONSTARTUP="$HOME/.pythonrc.py"
fi

if ! nc localhost 8124 < /dev/null; then
    polipo -c "$HOME/.polipo/config" &
fi

export PYTHONPATH=~/code/miyamoto:~/code/hanzo-warc-tools
export LESSOPEN="| $HOME/bin/lesspipe.sh %s"
export LESS="-RFNJX"
export MANPAGER="less -sn" # disable line numbers in man, they're meaningless and mess up the formatting
