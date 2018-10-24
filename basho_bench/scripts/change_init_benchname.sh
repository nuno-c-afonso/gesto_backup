#!/bin/bash

set -u
set -e

# Different users need different node names
USERS="local root causal eventual"

FAIL=0
Counter=0
nodes=`cat ./scripts/bench`
config=`cat ./scripts/config`

for user in $USERS
do
    let Counter=Counter+1
    for node in $nodes
    do
        NodeName="basho_bench$Counter@$node"
        Command2="cd ./basho_bench && sudo sed -i -e \"s/{saturn_mynode.*/{saturn_mynode, ['$NodeName', longnames]}./\" examples/$config"
        Command2="$Command2 && erl -pa script -name $NodeName -setcookie saturn_leaf -run init stop"
        ssh -o ConnectTimeout=10 -t $user@$node ${Command2/localhost/$node} &
    done
done

echo $Command2 done

for job in `jobs -p`
do
    wait $job || let "FAIL+=1"
done

if [ "$FAIL" == "0" ];
then
echo "$Command2 finished." 
else
echo "Fail! ($FAIL)"
fi
