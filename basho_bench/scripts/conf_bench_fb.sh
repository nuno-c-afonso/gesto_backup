#!/bin/bash

Duration=$1
Value=$2
Keys=$3
Clients=$4
Driver=$5
KeyDistribution=$6
Masters=$7
Friends=$8

./scripts/set_bench_param.sh "{duration.*/{duration, $Duration}"
./scripts/set_bench_param.sh "{driver.*/{driver, $Driver}"
./scripts/set_bench_param.sh "{concurrent.*/{concurrent, $Clients}"
./scripts/set_bench_param.sh "{value_generator.*/{value_generator, {fixed_bin, $Value}}"
./scripts/set_bench_param.sh "{key_generator.*/{key_generator, {$KeyDistribution, $Keys}}"
./scripts/set_bench_param_tag.sh "{saturn_masters_file.*#{saturn_masters_file, '../../files/$Masters'}"
./scripts/set_bench_param_tag.sh "{saturn_graph_file.*#{saturn_graph_file, '../../files/$Friends'}"
./scripts/set_bench_param.sh "{operations.*/{operations, [{update, 179},{read, 9821}]}"
