#!/bin/bash

./scripts/change_benchname.sh

./scripts/init_name_bench.sh

Counter=0
nodes=`cat ./scripts/bench`
nodesArray=(`cat ./scripts/leafs`)
receiversArray=(`cat ./scripts/receivers`)

for node in $nodes
do
    let Counter=Counter+1
    let Index=Counter-1
    NodeName="leafs$Counter@${nodesArray[$Index]}"
    ReceiverName="receivers$Counter@${receiversArray[$Index]}"
    echo $NodeName
    ./scripts/set_bench_dc.sh $node "{saturn_dc_nodes.*/{saturn_dc_nodes, ['$NodeName']}"
    ./scripts/set_bench_dc.sh $node "{saturn_dc_receiver.*/{saturn_dc_receiver, ['$ReceiverName']}"
    ./scripts/set_bench_dc.sh $node "{saturn_dc_id.*/{saturn_dc_id, $Index}"
done
