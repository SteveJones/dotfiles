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

# Check for broken term
if ! tput longname > /dev/null; then
    case "$TERM" in
	rxvt-unicode-256color)
	    if tput -Trxvt-256color longname > /dev/null; then
		export TERM=rxvt-256color
	    elif tput -Trxvt-unicode longname > /dev/null; then
		export TERM=rxvt-unicode
	    elif tput -Trxvt longname > /dev/null; then
		export TERM=rxvt
	    else
		# Where in the hell are you?
		export TERM=vt100
		export TERM_BROKEN=1
	    fi
	    ;;
	*)
	    export TERM_BROKEN=1
    esac
fi

if [ -z "$TERM_BROKEN" ]; then
    export MAX_COLOURS="$(tput colors)"
else
    if [ -z "$INSIDE_EMACS" ]; then
	export MAX_COLOURS=2
    else
	export MAX_COLOURS=8
    fi
fi

function rgb_color {
    local RED=$1
    local GREEN=$2
    local BLUE=$3

    case $MAX_COLOURS in
	256)
	    RED=$(((RED * 5 + 50) / 100))
	    GREEN=$(((GREEN * 5 + 50) / 100))
	    BLUE=$(((BLUE * 5 + 50) / 100))
	    COLOR=$((16 + (RED * 6 + GREEN) * 6 + BLUE))
	    ;;
	88)
	    RED=$(((RED * 3 + 50) / 100))
	    GREEN=$(((GREEN * 3 + 50) / 100))
	    BLUE=$(((BLUE * 3 + 50) / 100))
	    COLOR=$((16 + (RED * 4 + GREEN) * 4 + BLUE))
	    ;;
	8|16)
	    RED=$(((RED + 50) / 100))
	    GREEN=$(((GREEN + 50) / 100))
	    BLUE=$(((BLUE + 50) / 100))
	    COLOR=$(((BLUE * 2 + GREEN) * 2 + RED))
	    ;;
	default)
	    # This function only works for color mode
	    return
    esac

    if [ "x$4" = "xbold" ]; then
	echo '\[\033[01;38;5;'$((COLOR))'m\]'
    else
	echo '\[\033[00;38;5;'$((COLOR))'m\]'
    fi
}

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

function bzr_root {
    VC_ROOT="$(up_till_file .bzr)"
    return $?
}

function vc_root {
    if hg_root; then
	VC_TYPE="hg"
    elif git_root; then
	VC_TYPE="git"
    elif bzr_root; then
	VC_TYPE="bzr"
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

function hg_branch {
    hg branch
}

function git_branch {
    git branch | grep '\*' | cut -c 3-
}

function git_changed {
    local CHANGES
    CHANGES="$(git status --porcelain)"
    if [ $? -eq 0 ]; then
	local CH_COUNT="$(echo "$CHANGES" | grep '^[^?][^?]' | wc -l)"
	echo "$CH_COUNT"
    fi
}

function bzr_changed {
    local CHANGES
    CHANGES="$(bzr status -SV)"
    if [ $? -eq 0 ]; then
	local CH_COUNT="$(echo "$CHANGES" | grep . | wc -l)"
	echo "$CH_COUNT"
    fi
}

case $MAX_COLOURS in
    256|88|16|8)
	HOST_COLOUR="$(rgb_color 40 100 20)"
	PATH_COLOUR="$(rgb_color 80 100 100)"
	PATH_HI_COLOUR="$(rgb_color 40 100 100)"
	PATH_SEP_COLOUR="$(rgb_color 100 80 80)"
	STATUS_COLOUR="$(rgb_color 80 80 00)"
	BRANCH_COLOUR="$(rgb_color 80 60 80)"
	ERROR_COLOUR="$(rgb_color 100 20 00 bold)"
	;;
    *)
	HOST_COLOUR="\[\033[00;32m\]"
	PATH_COLOUR="\[\033[00;36m\]"
	PATH_HI_COLOUR="\[\033[01;36m\]"
	PATH_SEP_COLOUR="\[\033[00m\]"
	STATUS_COLOUR="\[\033[00;33m\]"
	BRANCH_COLOUR="\[\033[00;31m\]"
	ERROR_COLOUR="\[\033[01;31m\]"
esac

function ps1_path_update {
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
    local ERROR=$?
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
    if [ "$VC_TYPE" = "hg" ]; then
	PS1_STATUS="$PATH_SEP_COLOUR(${BRANCH_COLOUR}$(hg_branch)$PATH_SEP_COLOUR:$STATUS_COLOUR$(hg_changed)$PATH_SEP_COLOUR)"
    elif [ "$VC_TYPE" = "git" ]; then
	PS1_STATUS="$PATH_SEP_COLOUR(${BRANCH_COLOUR}$(git_branch)$PATH_SEP_COLOUR:$STATUS_COLOUR$(git_changed)$PATH_SEP_COLOUR)"
    elif [ "$VC_TYPE" = "bzr" ]; then
	PS1_STATUS="$PATH_SEP_COLOUR($(bzr_changed)$PATH_SEP_COLOUR)"
    fi

    local PS1_HOST=""
    if [ "x$SSH_CLIENT" != "x" ]; then
	PS1_HOST="$HOST_COLOUR\h"
    fi

    if [ "$ERROR" -eq 0 ]; then
	PS1="$PS1_HOST$PS1_PATH$PS1_STATUS\[\033[00m\]\$ "
    else
	PS1="$PS1_HOST$PS1_PATH$PS1_STATUS$ERROR_COLOUR<$ERROR>\[\033[00m\]\$ "
    fi
}

function s {
    if [ "x" == "x$1" ]; then
	ls -lh --color=auto
	return
    fi
    TYPE="$(file -b --mime-type "$1")"
    case "$TYPE" in
	application/x-directory)
	    ls -lh --color=auto "$1"
	    ;;
	text/*)
	    less "$1"
	    ;;
	*)
	    echo "unknown file type $TYPE" 1>&2
    esac
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

export PYTHONPATH=~/code/miyamoto:~/code/hanzo-warc-tools
export PYTHONSTARTUP=~/.pythonrc.py

export PATH="$HOME/bin:$HOME/.local/bin:$PATH"

export LESSOPEN="| $HOME/bin/lesspipe.sh %s"
export LESS="-RFNJ"
export MANPAGER="less -sn" # disable line numbers in man, they're meaningless and mess up the formatting

bind '"\e/":dabbrev-expand'
