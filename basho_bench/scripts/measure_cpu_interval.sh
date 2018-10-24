#!/bin/bash

# Interval to take the measurements in seconds
INTERVAL=1

while true
do
    # Take the initial timestamp for the sleep time calculations
    before=$(date '+0.%3N')

    # Take the numbers
    load=$(./current_cpu_load.sh)
    ts=$(date '+%H:%M:%S')
    
    # Present them in a single line
    echo "$ts $load"

    # Calculate the time for sleeping
    after=$(date '+0.%3N')
    diff=$(echo "$after-$before" | bc | sed "s|-||g")
    to_sleep=$(echo "$INTERVAL-$diff" | bc)

    # Wait for the next measurement
    sleep $to_sleep
done