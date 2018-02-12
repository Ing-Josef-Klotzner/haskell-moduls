#!/bin/bash
longest=9
shortest=5
rm dict_5_9.txt 2> /dev/null
for word in $(<dict.txt)
do
    len=${#word}
    if (( len <= longest && len >= shortest ))
    then
        echo $word >> dict_5_9.txt
    fi
done
printf 'The initial 'dict.txt' is written to 'dict_5_9.txt' shortest word is %d, longest %d.\n' "$shortest" "$longest"
