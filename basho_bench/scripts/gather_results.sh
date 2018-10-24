#!/bin/bash

set -u
set -e
FAIL=0
if [ $# -eq 1 ]
then
    Folder=$1
else
    Folder="current"
fi
Nodes=`cat ./scripts/bench`
Folder=`date +"%d%m%y_%H%M%S"`
mkdir ./results/$Folder
for node in $Nodes
do
    mkdir ./results/$Folder/$node

    # There will be four locations per machine
    mkdir ./results/$Folder/$node/root
    scp -r   root@$node:basho_bench/tests/current/* ./results/$Folder/$node/root &
    mkdir ./results/$Folder/$node/local
    scp -r   root@$node:/home/local/basho_bench/tests/current/* ./results/$Folder/$node/local &
    mkdir ./results/$Folder/$node/eventual
    scp -r   root@$node:/home/eventual/basho_bench/tests/current/* ./results/$Folder/$node/eventual &
    mkdir ./results/$Folder/$node/causal
    scp -r   root@$node:/home/causal/basho_bench/tests/current/* ./results/$Folder/$node/causal &
done
rm -rf ./results/current
ln -s $Folder/ ./results/current
command="scp & ln"

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
