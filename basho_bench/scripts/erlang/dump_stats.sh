#!/bin/bash

FILE_OUT=$1

Leafs=`cat ../leafs`
Ip=`dig +short myip.opendns.com @resolver1.opendns.com`

# Sequential version
# sudo erl -pa script -name "stat@$Ip" -setcookie saturn_leaf -run stats dump_stats $Leafs -run init stop >> $FILE_OUT

# Send the requests in parallel fashion
leaf_id=1
for leaf in $Leafs
do
    temp_file="$leaf.temp"
    sudo erl -noshell -pa script -name "stat$leaf_id@$Ip" -setcookie saturn_leaf -run stats dump_stats_parallel $leaf $leaf_id $Leafs -run init stop > $temp_file &
    let leaf_id++
done

# Wait until all responses are received
running=$(jobs -r | wc -l)
while [ $running -ne 0 ]
do
    running=$(jobs -r | wc -l)
done

# Join the results in a single file
for leaf in $Leafs
do
    temp_file="$leaf.temp"
    cat $temp_file >> $FILE_OUT
    rm $temp_file
done
