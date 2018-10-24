#!/bin/bash

Duration=$1
Value=$2
Correlation=$3
Keys=$4
Update=$5
UpdateRemote=$6
Read=$7
ReadRemote=$8
DoubleMigDiff=$9
DoubleMigSame=${10}
Clients=${11}
Driver=${12}
ReadTx=${13}
NKeysTx=${14}
TxRemote=${15}
WriteTx=${16}
KeyDistribution=${17}
User=${18}

./scripts/set_bench_param.sh "{duration.*/{duration, $Duration}" $User
./scripts/set_bench_param.sh "{driver.*/{driver, $Driver}" $User
./scripts/set_bench_param.sh "{concurrent.*/{concurrent, $Clients}" $User
./scripts/set_bench_param.sh "{value_generator.*/{value_generator, {fixed_bin, $Value}}" $User
./scripts/set_bench_param.sh "{saturn_correlation.*/{saturn_correlation, $Correlation}" $User
#./scripts/set_bench_param.sh "{saturn_number_internalkeys.*/{saturn_number_internalkeys, $Keys}" $User
./scripts/set_bench_param.sh "{operations.*/{operations, [{update, $Update},{remote_update, $UpdateRemote},{read, $Read}, {remote_read, $ReadRemote}, {double_migration_different, $DoubleMigDiff}, {double_migration_same, $DoubleMigSame}, {read_tx, $ReadTx}, {write_tx, $WriteTx}]}" $User
#./scripts/set_bench_param.sh "{operations.*/{operations, [{update, $Update},{remote_update, $UpdateRemote},{read, $Read}]}" $User
#./scripts/set_bench_param.sh "{operations.*/{operations, [{update, $Update},{read, $Read}]}" $User
#./scripts/set_bench_param.sh "{operations.*/{operations, [{read, $Read}]}" $User
#/scripts/set_bench_param.sh "{operations.*/{operations, [{update, 1}]}" $User
./scripts/set_bench_param.sh "{saturntx_n_key.*/{saturntx_n_key, $NKeysTx}" $User
./scripts/set_bench_param.sh "{saturntx_remote_percentage.*/{saturntx_remote_percentage, $TxRemote}" $User
./scripts/set_bench_param.sh "{key_generator.*/{key_generator, {$KeyDistribution, $Keys}}" $User
