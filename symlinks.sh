#!/bin/bash

for f in $(find ./_obuild -iname "*.asm" -executable)
do
    filename=$(basename "$f")
    s=${f##*/}
    s=${s%.*}
    ln -s "$f" "$s.native"
done
