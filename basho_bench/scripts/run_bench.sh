#ol!/bin/bash

#Duration=$1
#Value=$2
#Correlation=$3
#Keys=$4
#Update=$5
#UpdateRemote=$6
#Read=$7
#ReadRemote=$8
#DoubleMigDiff=$9
#DoubleMigSame=${10}
#Clients=${11}
#Driver=${12}
#ReadTx=${13}
#NKeysTx=${14}
#TxRemote=${15}
#WriteTx=${16}
#KeyDistribution=${17}

./scripts/conf_bench.sh 2 2 uniform 10000 1 0 90 4 2 3 1 saturn_benchmarks_da_migration 0 0 0 0 uniform_int

Counter=0
nodes=`cat ./scripts/bench`
nodesArray=(`cat ./scripts/leafs`)
NLeaves=(`echo $nodes | wc -w`)
receiversArray=(`cat ./scripts/receivers`)

# Gets the same list for all clients
let Index=0
let Counter=Index+1
NodesName="'leafs$Counter@${nodesArray[$Index]}'"
let Index=Index+1
while [ $Index -lt $NLeaves ]
do
    let Counter=Index+1
    NodesName="$NodesName,'leafs$Counter@${nodesArray[$Index]}'"
    let Index=Index+1
done

Counter=0
for node in $nodes
do
    let Counter=Counter+1
    let Index=Counter-1    
    
    ReceiverName="receivers$Counter@${receiversArray[$Index]}"
    echo $NodeName
    ./scripts/set_bench_dc.sh $node "{saturn_dc_nodes.*/{saturn_dc_nodes, [$NodesName]}"
    ./scripts/set_bench_dc.sh $node "{saturn_dc_receiver.*/{saturn_dc_receiver, ['$ReceiverName']}"
    ./scripts/set_bench_dc.sh $node "{saturn_dc_id.*/{saturn_dc_id, $Index}"
done

Command1="sudo /usr/sbin/ntpdate -b ntp.ubuntu.com"
./scripts/parallel_command_all.sh "$Command1"

Command2="cd ./basho_bench && sudo ./basho_bench examples/saturn_benchmarks_rpc.config"

./scripts/parallel_command.sh bench "$Command2"
