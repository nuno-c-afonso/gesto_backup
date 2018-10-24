#!/bin/bash

set -u
set -e
FAIL=0
command="$1"

leafs=`cat ./scripts/leafs`
internals=`cat ./scripts/internals`
receivers=`cat ./scripts/receivers`
bench=`cat ./scripts/bench`

############
## LEAVES ##
############
echo $command" for leafs:"$leafs 
for node in $leafs
do
    ssh -o StrictHostKeyChecking=no -o ConnectTimeout=10 -t root@$node   ${command/localhost/$node} & 
done

###############
## INTERNALS ##
###############
echo $command" for internals:"$internals 
for node in $internals
do
    ssh -o StrictHostKeyChecking=no -o ConnectTimeout=10 -t root@$node   ${command/localhost/$node} &
done

###############
## RECEIVERS ##
###############
echo $command" for receivers:"$receivers 
for node in $receivers
do
    ssh -o StrictHostKeyChecking=no -o ConnectTimeout=10 -t root@$node   ${command/localhost/$node} &
done
echo $command done

###########
## BENCH ##
###########
echo $command" for bench:"$bench 
for node in $bench
do
    ssh -o StrictHostKeyChecking=no -o ConnectTimeout=10 -t root@$node   ${command/localhost/$node} &
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
