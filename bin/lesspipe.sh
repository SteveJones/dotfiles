#!/bin/bash

NAME="$1"

if ! [ -r "$NAME" ]; then
    exit 1
fi

if [ -d "$NAME" ]; then
    ls -l --color=always "$NAME"
    exit
fi

GEN_CASE=$(cat <<EOF
/^\*.*:/ {
    match(\$0, "\\\\* [^ ,:]+");
    LEX = substr(\$0, RSTART + 2, RLENGTH - 2);
}

/filenames/ {
    match(\$0, "filenames [^)]+");
    PATTERN = substr(\$0, RSTART + 10, RLENGTH - 10)
    gsub(", *", "|", PATTERN);
    printf "%s) pygmentize -O style=monokai -f terminal256 -l \\"%s\\" 2> /dev/null ;;\\n", PATTERN, LEX;
}
EOF
)

case "$NAME" in
    *.deb)
	CMD="dpkg --info \"$NAME\" && echo && dpkg -c \"$NAME\""
	;;
    *.tar|*.tar.gz)
	CMD="gunzip -c \"$NAME\" | tar -t"
	;;
    *.gz)
	CMD="gunzip -c \"$NAME\""
	NAME="${NAME%.gz}"
	;;
    *)
	CMD="cat \"$NAME\""
esac

NAME="${NAME##*/}"

CASE="$(pygmentize -L lexers | awk "$GEN_CASE")"

bash <<EOF
$CMD | case "$NAME" in
rfc*.txt)
  tr '\f' '\n'
  ;;
.bashrc)
  pygmentize -O style=monokai -f terminal256 -l bash
  ;;
$CASE
*)
  cat
  ;;
esac
EOF
