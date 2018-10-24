#!/bin/bash

./scripts/set_bench_param.sh "{saturn_bucket_full.*/{saturn_bucket_full, $1}"
./scripts/set_bench_param_tag.sh "{saturn_tree_file.*#{saturn_tree_file, '$2'}"
