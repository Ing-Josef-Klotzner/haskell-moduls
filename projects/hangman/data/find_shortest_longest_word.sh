#!/bin/bash
longest=0
shortest=50
for word in $(<dict.txt)
do
    len=${#word}
    if (( len > longest ))
    then
        longbefore=$longest
        longbeforewd=$longword
        longest=$len
        longword=$word
    fi
    if (( len < shortest ))
    then
        shortbefore=$shortest
        shortbeforewd=$shortword
        shortest=$len
        shortword=$word
    fi
done
printf 'The longest word is %s and its length is %d.\n' "$longword" "$longest"
printf 'The longestbefore word is %s and its length is %d.\n' "$longbeforewd" "$longbefore"
printf 'The shortest word is %s and its length is %d.\n' "$shortword" "$shortest"
printf 'The shortestbefore word is %s and its length is %d.\n' "$shortbeforewd" "$shortbefore"
