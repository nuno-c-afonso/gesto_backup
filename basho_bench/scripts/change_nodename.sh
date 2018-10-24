#!/bin/bash

set -u
set -e
Type=$1
FAIL=0
Counter=0
nodes=`cat ./scripts/$Type`
for node in $nodes
do
    let Counter=Counter+1
    NodeName="$Type$Counter@$node"
    Command2="cd ./saturn_leaf && sudo sed -i -e \"s/{node.*/{node, '$NodeName'}./\" rel/vars.config"
    ssh -o ConnectTimeout=10 -t root@$node   ${Command2/localhost/$node} &
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
