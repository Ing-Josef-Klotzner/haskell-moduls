#!/bin/bash
T=""
for i in `seq 1 10000`;
do
    T+="10 "
done    
echo $T > SubsetSum_test
