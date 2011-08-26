#! /bin/sh

v="$(ocamlfind query -format '%v' netstring)"
case "$v" in
    [3456789].*)
	exit 0 ;;
    [012].*)
	exit 1 ;;
esac
