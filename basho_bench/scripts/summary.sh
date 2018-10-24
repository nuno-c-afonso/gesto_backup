#!/bin/bash

./scripts/gather_results.sh
./scripts/merge_thput.sh
cat results/current/summary.csv
