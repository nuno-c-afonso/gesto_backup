#!/bin/bash

Command0="cd ./saturn_leaf/deps/saturn_groups_manager && git pull origin master && make rel"
Command1="cd ./saturn_leaf/deps/saturn_internal/deps/saturn_groups_manager && git pull origin master && make rel"

./scripts/parallel_command_all.sh "$Command0"
./scripts/parallel_command_all.sh "$Command1"
