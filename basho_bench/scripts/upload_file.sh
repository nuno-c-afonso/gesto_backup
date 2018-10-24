#!/bin/bash

set -u
set -e
FAIL=0
command="updaloading: $1"
leafs=`cat ./scripts/leafs`
internals=`cat ./scripts/internals`
config="data/manager/$1"
echo $command" for leafs:"$leafs 
for node in $leafs
do
    scp -o ConnectTimeout=10   files/saturn/"$1" root@$node:saturn_leaf/rel/files/manager/ & 
    Command2="cd ./saturn_leaf && sudo sed -i -e 's#{tree.*#{tree, \"$config\"},#' src/saturn_leaf.app.src"
    ssh -o ConnectTimeout=10 -t root@$node   ${Command2/localhost/$node} &
done
echo $command" for internals:"$internals 
for node in $internals
do
    scp -o ConnectTimeout=10   files/saturn/"$1" root@$node:saturn_leaf/rel/files/manager/ & 
    Command2="cd ./saturn_leaf && sudo sed -i -e 's#{tree.*#{tree, \"$config\"},#' src/saturn_leaf.app.src"
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
