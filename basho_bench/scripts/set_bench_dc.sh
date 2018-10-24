#!/bin/bash

set -u
set -e

USERS="local root causal eventual"

FAIL=0
config=`cat ./scripts/config`
Command1="cd ./basho_bench && sudo sed -i -e \"s/$2./\" examples/$config"

for user in $USERS;
do
    ssh -o ConnectTimeout=10 -t $user@$1 ${Command1/localhost/$1} &
done
echo $Command1 done

for job in `jobs -p`
do
    wait $job || let "FAIL+=1"
done

if [ "$FAIL" == "0" ];
then
echo "$Command1 finished." 
else
echo "Fail! ($FAIL)"
fi
