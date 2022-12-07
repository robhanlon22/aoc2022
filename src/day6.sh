#!/usr/bin/env bash

s="$(< /dev/stdin)"
n="$1"

for (( i = 0; i < ${#s} - n; i++ )); do
    unset m
    declare -A m

    for (( j = 0; j < n; j++ )); do
        m[${s:$((i + j)):1}]=$j
    done

    if ((${#m[@]} == n )); then
        echo $((i + n))
        exit
    fi
done

echo "it's not possible :(" >&2
exit 1
