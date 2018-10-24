#!/bin/bash

set -u
set -e
FAIL=0
command="setting heartbeat frequency to: $1"
leafs=`cat ./scripts/leafs`
internals=`cat ./scripts/internals`
echo $command" for leafs:"$leafs 
for node in $leafs
do
    Command2="cd ./saturn_leaf && sudo sed -i -e 's#-define(HEARTBEAT_FREQ.*#-define(HEARTBEAT_FREQ, $1).#' include/saturn_leaf.hrl"
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
