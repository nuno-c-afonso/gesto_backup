#!/bin/bash

nodes=(`cat ./scripts/$1`)
    
command="cat ./saturn_leaf/rel/saturn_leaf/log/console.log"
node="${nodes[$2]}"

ssh -o StrictHostKeyChecking=no -o ConnectTimeout=10 -t root@$node   ${command/localhost/$node} &
