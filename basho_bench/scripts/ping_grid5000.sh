#!/bin/bash

# Get addresses
addresses=$(cat scripts/internals scripts/leafs scripts/bench)

mkdir ping_res
cd ping_res

# Match the machines with each other
for origin in $addresses
do
    for dest in $addresses
    do
        if [ $origin != $dest ]
        then
            sleep 0.2
            ssh -o "StrictHostKeyChecking no" root@$origin "ping -c 100 -i 0.2 $dest | tail -1" &> $origin-$dest &
        fi
    done
done

# Wait until all the files are written
out=""
nr_addresses=$(echo $addresses | wc -w)
addresses=($(echo $addresses))
i=0
while [ $i -lt $nr_addresses ]
do
    j=0
    while [ $j -lt $nr_addresses ]
    do
        if [ $i != $j ]; then
            f_name="${addresses[$i]}-${addresses[$j]}"
            f_content=$(cat $f_name)
            if [[ -z "${f_content// }" ]]; then
                continue
            else
                out="$out\n${addresses[$i]}->${addresses[$j]}: $f_content"
            fi
        fi
        let j=j+1
    done
    let i=i+1
done

cd ..
echo -e $out | tee "results/$(date +%Y-%m-%d_%H:%M).ping"
rm -r ping_res
