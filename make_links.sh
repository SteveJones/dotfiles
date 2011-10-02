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
