#!/bin/bash

case "$1" in
    *.py|*.sql|*.c|*.h|*.cpp|*.hpp|*.pl|*.sh|*.cl|*.el)
	pygmentize -f terminal256 "$1"
	;;
esac
