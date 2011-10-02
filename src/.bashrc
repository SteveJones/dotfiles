# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# don't put duplicate lines in the history. See bash(1) for more options
# ... or force ignoredups and ignorespace
HISTCONTROL=ignoredups

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    screen-bce) color_prompt=yes;;
    xterm-color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
#force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
fi

function up_till_file {
    if [ "$(pwd)" = "/" ]; then
	return 1
    fi
    if [ -e "$1" ]; then
	pwd
	return 0
    fi
    pushd .. > /dev/null
    up_till_file "$1"
    local RET=$?
    popd > /dev/null
    return $RET
}

function hg_root {
    VC_ROOT="$(hg root 2> /dev/null)"
    return $?
}

function git_root {
    VC_ROOT="$(up_till_file .git)"
    return $?
}

function vc_root {
    if hg_root; then
	VC_TYPE="hg"
    elif git_root; then
	VC_TYPE="git"
    else
	VC_TYPE="none"
    fi
}

function hg_changed {
    local CHANGES
    CHANGES="$(hg status -m -a -r -d 2> /dev/null)"
    if [ $? -eq 0 ]; then
	local CH_COUNT="$(echo "$CHANGES" | grep . | wc -l)"
	echo "$CH_COUNT"
    fi
}

function git_changed {
    local CHANGES
    CHANGES="$(git status --porcelain)"
    if [ $? -eq 0 ]; then
	local CH_COUNT="$(echo "$CHANGES" | grep -v '^??' | wc -l)"
	echo "$CH_COUNT"
    fi
}

function ps1_path_update {
    local PATH_COLOUR="\[\033[00;36m\]"
    local PATH_HI_COLOUR="\[\033[01;36m\]"
    local PATH_SEP_COLOUR="\[\033[00m\]"
    if [ "x$VC_ROOT" = "x" ]; then
	local P="${PWD/$HOME/~}"
	P="${P//\//$PATH_SEP_COLOUR/$PATH_COLOUR}"
	echo "$PATH_COLOUR$P"
    else
	local VC_PARENT="${VC_ROOT%/*}"
	local VC_PATH=${PWD#$VC_PARENT}
	VC_PARENT="${VC_PARENT/$HOME/~}"
	VC_PARENT="${VC_PARENT//\//$PATH_SEP_COLOUR/$PATH_COLOUR}"
	VC_PATH="${VC_PATH/$HOME/~}"
	VC_PATH="${VC_PATH//\//$PATH_SEP_COLOUR/$PATH_HI_COLOUR}"
	echo "$PATH_COLOUR$VC_PARENT$PATH_HI_COLOUR$VC_PATH"
    fi
}

_LAST_PWD="-"

function ps1_update {
    local PS1_PATH
    if [ "$_LAST_PWD" != "$PWD" ]; then
	vc_root
	PS1_PATH="$(ps1_path_update)"
	_LAST_PS1_PATH="$PS1_PATH"
	_LAST_PWD="$PWD"
    else
	PS1_PATH="$_LAST_PS1_PATH"
    fi

    local PS1_STATUS
    local STATUS_COLOUR="\[\033[00;33m\]"
    if [ "$VC_TYPE" = "hg" ]; then
	PS1_STATUS="$STATUS_COLOUR($(hg_changed))"
    elif [ "$VC_TYPE" = "git" ]; then
	PS1_STATUS="$STATUS_COLOUR($(git_changed))"
    fi

    local HOST_COLOUR="\[\033[00;32m\]"
    local PS1_HOST=""
    if [ "x$SSH_CLIENT" != "x" ]; then
	PS1_HOST="$HOST_COLOUR\h"
    fi

    PS1="$PS1_HOST$PS1_PATH$PS1_STATUS\[\033[00m\]\$ "
}

PROMPT_COMMAND="ps1_update"

if [ "$color_prompt" = yes ]; then
    PS1='\[\033[01;32m\]\u@\h\[\033[00;36m\]\$VC_ROOT\[\033[01;36m\]$(vc_path)$(hg_changed)\[\033[00m\]\$ '
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
*)
    ;;
esac

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi

export PYTHONPATH=~/code/miyamoto
export PYTHONSTARTUP=~/.pythonrc.py

export PATH="$HOME/bin:$PATH"

export LESSOPEN="| $HOME/bin/lesspipe.sh %s"
export LESS="-R"
