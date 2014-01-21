#!/bin/bash

for file in src/.*; do
    if [ -f "$file" ]; then
	base="${file##*/}"
	target="$HOME/$base"
	if [ -h "$target" ]; then
	    rm $target;
	elif [ -f "$target" ]; then
	    mv "$target" "$target.bak"
	fi
	ln -s "$PWD/$file" "$target"
    fi
done

if [ -h "$HOME/bin" ]; then
    rm "$HOME/bin"
elif [ -d "$HOME/bin" ]; then
    mv "$HOME/bin" "$HOME/bin.bak"
fi

ln -s "$PWD/bin" "$HOME/bin"

if ! [ -d "$HOME/.emacs.d" ]; then
    mkdir "$HOME/.emacs.d"
fi

if [ -h "$HOME/.emacs.d/elisp" ]; then
    rm "$HOME/.emacs.d/elisp"
elif [ -d "$HOME/.emacs.d/elisp" ]; then
    mv "$HOME/.emacs.d/elisp" "$HOME/.emacs.d/elisp.bak"
fi

ln -s "$PWD/elisp" "$HOME/.emacs.d/elisp"

