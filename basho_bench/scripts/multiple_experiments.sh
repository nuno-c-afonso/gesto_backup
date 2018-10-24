#ol!/bin/bash

#Duration=$1
#Value=$2
#Correlation=$3
#Keys=$4
#Update=$5
#UpdateRemote=$6
#Read=$7
#ReadRemote=$8
#Clients=$9
#Driver=${10}
#ReadTx=${11}
#NKeysTx=${12}
#TxRemote=${13}
#WriteTx=${14}
#KeyDistribution=${15}

echo "gentlerain thput 6dcs"

./scripts/conf_bench.sh 3 2 full 1000000 10 0 90 0 12 saturn_benchmarks_gentlerain_da 0 0 0 0 uniform_int
./scripts/run_bench_short.sh
./scripts/summary.sh

echo "running 10%"

./scripts/conf_bench.sh 5 2 exponential 10000 10 0 80 10 11 saturn_benchmarks_cure_da 0 0 0 0 uniform_int
./scripts/run_bench_short.sh
./scripts/summary.sh

echo "running 20%"

./scripts/conf_bench.sh 5 2 exponential 10000 10 0 70 20 10 saturn_benchmarks_cure_da 0 0 0 0 uniform_int
./scripts/run_bench_short.sh
./scripts/summary.sh

echo "running 40%"

./scripts/conf_bench.sh 5 2 exponential 10000 10 0 50 40 8 saturn_benchmarks_cure_da 0 0 0 0 uniform_int
./scripts/run_bench_short.sh
./scripts/summary.sh


echo "512B"

./scripts/conf_bench.sh 5 128 exponential 10000 10 0 90 0 10 saturn_benchmarks_cure_da 0 0 0 0 uniform_int
./scripts/run_bench_short.sh
./scripts/summary.sh

