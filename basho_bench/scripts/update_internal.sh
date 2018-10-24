#!/bin/bash

Command0="cd ./saturn_leaf/deps/saturn_internal && git pull origin master && make rel"

./scripts/parallel_command_all.sh "$Command0"
