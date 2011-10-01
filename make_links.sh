#!/bin/bash

for file in src/.*; do
    if [ -f "$file" ]; then
	base="${file##*/}"
	target="$HOME/$base"
	if [ -e "$target" ]; then
	    mv "$target" "$target.bak"
	fi
	ln -s "$PWD/$file" "$target"
    fi
done
