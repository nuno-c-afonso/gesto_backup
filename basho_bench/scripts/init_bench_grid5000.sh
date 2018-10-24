#!/bin/bash

Command1="cd ./basho_bench && sudo make all"
./scripts/parallel_command_grid5000.sh bench "$Command1"
./scripts/change_init_benchname.sh
