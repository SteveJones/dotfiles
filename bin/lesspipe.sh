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
/^*.*:/ {
    match(\$0, "\\\\* ([^ ,:]+)", m);
    LEX=m[1];
}

/filenames/ {
    match(\$0, "filenames ([^)]+)", m);
    gsub(", *", "|",m[1]);
    printf "%s) pygmentize -f terminal256 -l \\"%s\\" ;;\\n", m[1], LEX;
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
$CASE
*)
  cat
  ;;
esac
EOF
