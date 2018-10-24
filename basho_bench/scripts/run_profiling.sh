#!/bin/bash

# Give the path to the log file as an argument
LOG_FILE=$1

erlang_script="eprof:start(). eprof:start_profiling(["
script_end="])."

function convert_pid_cmd {
    pid=$1
    echo "pid(0,$pid,0)"
}

# Get the PIDs
proxy_pids=$(cat $LOG_FILE | grep "saturn_proxy_vnode" | grep -Eo "<0\.[0-9]*\.0>" | grep -Eo "\.[0-9]*\." | grep -Eo "[0-9]*" | uniq)

# Build the Erlang script
for p in $proxy_pids
do
    converted_p=$(convert_pid_cmd $p)
    erlang_script="$erlang_script$converted_p,"
done

# Finish the script
erlang_script=${erlang_script::-1}
erlang_script="$erlang_script$script_end"

echo $erlang_script