#!/bin/bash

./scripts/gather_results.sh
./scripts/merge_thput.sh

# Read the remote update statistics
cd scripts/erlang
erlc *.erl
./compute_average_staleness.sh > ../../results/current/remote_update.txt
./dump_stats.sh ../../results/current/remote_update.txt
cd ../..

cat results/current/summary.csv
