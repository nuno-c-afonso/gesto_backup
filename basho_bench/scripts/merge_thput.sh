#!/bin/bash

if [ $# -eq 1 ]
then
    Root=$1
else
    Root="results/current"
fi
Folders=`ls -d $Root/*/`
Counter=0
Output=$Root/summary.csv
for F in $Folders
do
    for Dir in `ls -d $F*/`
    do
        let Counter=Counter+1
        File=$Dir"summary.csv"
        echo "$File"
        if [ $Counter -eq 1 ]
        then
            cp $File $Output
        else
            paste $Output $File | awk -F ',' '{print $1",",$2",",($3+$7)",",($4+$8)",",($5+$9)" "}' > $Root/tmp
            mv $Root/tmp $Output
        fi
    done
done
