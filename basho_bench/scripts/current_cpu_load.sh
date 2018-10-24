#!/bin/bash

#| PID | USER | PR | NI | VIRT | RES | SHR | S | %CPU | %MEM | TIME+ | COMMAND
COL_CPU=9
LINE_HEADER=8

usage=0

# Get the current system snapshot and remove the header info
while read -r line
do
    counter=0
    for el in $line
    do
        let counter=counter+1
        if [ $counter -eq $COL_CPU ]
        then
            # Convert the value to a valid format
            el=$(echo $el | sed "s|,|.|g")

            # Stop if there starts to appear 0.0
            if (( $(echo "0==$el" | bc -l) ))
            then
                # Output the calculated usage
                echo "$usage"
                exit
            fi

            # Add the percentage to the existing usage
            usage=$(echo "$usage+$el" | bc)
        fi
    done
done <<< $(top -n 1 -b -o +%CPU | tail +$LINE_HEADER)

# Output the calculated usage when it analyzed all lines
echo "$usage"
