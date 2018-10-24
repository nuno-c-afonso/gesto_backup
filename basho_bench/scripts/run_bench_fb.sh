#ol!/bin/bash

if [ $1 == "init" ]
then
    ./scripts/init_bench.sh
fi

#Duration=$1
#Value=$2
#Keys=$4
#Clients=$9
#Driver=${10}
#KeyDistribution=${15}
#masters file
#friends file

./scripts/conf_bench_fb.sh 3 2 10000 1 saturn_complex_gentlerain_rpc uniform_int masters_facebook_237.txt friends_facebook_237.txt

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

Command1="sudo /usr/sbin/ntpdate -b ntp.ubuntu.com"
./scripts/parallel_command_all.sh "$Command1"

Command2="cd ./basho_bench && sudo ./basho_bench examples/saturn_complex_rpc.config"

./scripts/parallel_command.sh bench "$Command2"
