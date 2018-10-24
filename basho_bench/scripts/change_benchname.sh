#!/bin/bash

set -u
set -e

# Needed for different users
user=$1

FAIL=0
Counter=0
nodes=`cat ./scripts/bench`
config=`cat ./scripts/config`
for node in $nodes
do
    let Counter=Counter+1
    NodeName="basho_bench$Counter@$node"
    Command2="cd ./basho_bench && sudo sed -i -e \"s/{saturn_mynode.*/{saturn_mynode, ['$NodeName', longnames]}./\" examples/$config"
    ssh -o ConnectTimeout=10 -t $user@$node ${Command2/localhost/$node} &
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
