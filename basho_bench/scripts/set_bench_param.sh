#!/bin/bash

set -u
set -e

if [ "$#" -gt 1 ]
then
    USERS=$2
else
    USERS="local root causal eventual"
fi

FAIL=0
Counter=0
nodes=`cat ./scripts/bench`
config=`cat ./scripts/config`
for node in $nodes
do
    for user in $USERS;
    do
        Command1="cd ./basho_bench && sudo sed -i -e \"s/$1./\" examples/$config"
        ssh -o ConnectTimeout=10 -t $user@$node ${Command1/localhost/$node} &
    done
done
echo $Command1 done

for job in `jobs -p`
do
    wait $job || let "FAIL+=1"
done

if [ "$FAIL" == "0" ];
then
echo "$Command1 finished." 
else
echo "Fail! ($FAIL)"
fi
