#!/bin/bash

set -u
set -e
FAIL=0
command="setting tree to: $1"
leafs=`cat ./scripts/leafs`
internals=`cat ./scripts/internals`
receivers=`cat ./scripts/receivers`
Command2="cd ./saturn_leaf && sudo sed -i -e 's#{tree.*#{tree, \"$1\"},#' src/saturn_leaf.app.src"
echo $command" for leafs:"$leafs 
for node in $leafs
do
    ssh -o ConnectTimeout=10 -t root@$node   ${Command2/localhost/$node} &
done
echo $command done
echo $command" for internals:"$internals 
for node in $internals
do
    ssh -o ConnectTimeout=10 -t root@$node   ${Command2/localhost/$node} &
done
echo $command done
echo $command" for receivers:"$receivers 
for node in $receivers
do
    ssh -o ConnectTimeout=10 -t root@$node   ${Command2/localhost/$node} &
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
