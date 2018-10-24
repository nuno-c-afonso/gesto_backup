#!/bin/bash

set -u
set -e
FAIL=0

USERS="local root causal eventual"

# This is the only option for calling the script
nodes=`cat ./scripts/$1`
command=$2

echo $command" for nodes:"$nodes 
for node in $nodes
do
    for user in $USERS;
    do
        ssh -o StrictHostKeyChecking=no -o ConnectTimeout=10 -t $user@$node ${command/localhost/$node} & 
    done
done
echo $command done

for job in `jobs -p`
do
    wait $job || let "FAIL+=1"
done

if [ "$FAIL" == "0" ];
then
echo "$command finished." 
else
echo "Fail! ($FAIL)"
fi
