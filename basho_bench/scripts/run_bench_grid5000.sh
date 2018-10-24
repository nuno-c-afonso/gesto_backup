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

# TODO: If running to get the latency per number of dependencies, use 5 minutes instead
# TODO: If running to get reconfiguration results, do NOT use clients with only local operations
# Different scripts for each user
./scripts/conf_bench.sh 2 2 uniform 10000 10 0 90 0 0 0 2 saturn_benchmarks_da_migration 0 0 0 0 uniform_int 'local'
# ./scripts/conf_bench.sh 2 2 uniform 10000 10 0 90 0 0 0 5 saturn_benchmarks_da_migration 0 0 0 0 uniform_int 'local'
# ./scripts/conf_bench.sh 2 2 uniform 10000 10 0 70 20 0 0 5 saturn_benchmarks_da_migration 0 0 0 0 uniform_int 'local'
# ./scripts/conf_bench.sh 2 2 uniform 10000 0 0 100 0 0 0 2 saturn_benchmarks_da_migration 0 0 0 0 uniform_int 'local'

# FIXME: This is just a test for reducing the load on the systems
# ./scripts/conf_bench.sh 2 2 uniform 10000 10 0 70 5 5 10 2 saturn_benchmarks_da_migration 0 0 0 0 uniform_int root
# ./scripts/conf_bench.sh 2 2 uniform 10000 0 0 0 100 0 0 2 saturn_benchmarks_da_migration_eventual 0 0 0 0 uniform_int eventual
# ./scripts/conf_bench.sh 2 2 uniform 10000 0 0 0 100 0 0 2 saturn_benchmarks_da_migration_causal 0 0 0 0 uniform_int causal

Counter=0
nodes=`cat ./scripts/bench`
nodesArray=(`cat ./scripts/leafs`)
NLeaves=(`cat ./scripts/leafs | wc -l`)
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

    # Adapted for the Scaling evaluation
    Index=$(($Index % $NLeaves))

    ./scripts/set_bench_dc.sh $node "{saturn_dc_nodes.*/{saturn_dc_nodes, [$NodesName]}"
    ./scripts/set_bench_dc.sh $node "{saturn_dc_receiver.*/{saturn_dc_receiver, ['$ReceiverName']}"
    ./scripts/set_bench_dc.sh $node "{saturn_dc_id.*/{saturn_dc_id, $Index}"
done

Command1="sudo /usr/sbin/ntpdate -b ntp.ubuntu.com"
./scripts/parallel_command_all_with_bench.sh "$Command1"

# TODO: If running to get reconfiguration results for Gesto reconfiguration, uncomment the following section
#################################
## RECONFIGURATION EXPERIMENTS ##
#################################
# sudo make all # The manager is the only client that was not compiled
# sudo sed -i -e "s/{saturn_dc_nodes.*/{saturn_dc_nodes, [$NodesName]}\./" examples/reconfig_manager.config
# manager_ip=$(dig +short myip.opendns.com @resolver1.opendns.com)
# sudo sed -i "s|reconfig@manager|reconfig@$manager_ip|g" examples/reconfig_manager.config
# sudo ./basho_bench examples/reconfig_manager.config &

Command2="cd ./basho_bench && sudo ./basho_bench examples/saturn_benchmarks_rpc.config"

./scripts/parallel_command_grid5000.sh bench "$Command2"
