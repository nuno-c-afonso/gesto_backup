#!/bin/bash

set -u
set -e
FAIL=0
command="setting zdbbl to: $1"
leafs=`cat ./scripts/leafs`
internals=`cat ./scripts/internals`
echo $command" for leafs:"$leafs 
for node in $leafs
do
    Command2="cd ./saturn_leaf && sudo sed -i -e 's#+zdbbl.*#+zdbbl $1#' rel/files/vm.args"
    ssh -o ConnectTimeout=10 -t root@$node   ${Command2/localhost/$node} &
done
echo $command" for leafs:"$internals 
for node in $internals
do
    Command2="cd ./saturn_leaf && sudo sed -i -e 's#+zdbbl.*#+zdbbl $1#' rel/files/vm.args"
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
