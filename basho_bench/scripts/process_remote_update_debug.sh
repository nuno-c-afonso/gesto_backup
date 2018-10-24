#!/bin/bash

FILE="../grid5000_res/remote_update_diff4.debug"
FILES_LEAVES="../grid5000_res/remote_update_diff4_51-2.debug ../grid5000_res/remote_update_diff4_130-37.debug ../grid5000_res/remote_update_diff4_97-5.debug"
FILE_INTERNAL="../grid5000_res/remote_update_diff.debug"

# It must receive the numbers
process_numbers () {
    STEP=1
    PRINT=90
    HALF=50
    END=100

    numbers=$1
    counter=0
    total=0
    min=99999999999
    max=-1

    for n in $numbers
    do
        let counter=counter+1
        let total=total+$n

        # Update max
        if [ $n -gt $max ]
        then
            max=$n
        fi

        # Update min
        if [ $n -lt $min ]
        then
            min=$n
        fi
    done

    # First part of the output
    average=$(bc -l <<< "scale=4;$total/$counter/1000")
    min=$(bc -l <<< "scale=4;$min/1000")
    max=$(bc -l <<< "scale=4;$max/1000")
    echo -e "\t Average: $average ms"
    echo -e "\t Min: $min ms"
    echo -e "\t Max: $max ms"

    # Order numbers for percentiles
    numbers=($(for i in $numbers; do echo $i; done | sort -g))

    nr_elements=$counter
    counter=0
    while [ $counter -lt $END ]
    do
        let counter=counter+STEP        
        if [ $counter -gt $PRINT ] || [ $counter -eq $HALF ]
        then
            let index=nr_elements*counter/END-1
            value=$(bc -l <<< "scale=4;${numbers[$index]}/1000")
            echo "$counter%: $value ms"
        fi
    done
}

##########################
## Origin dispatch time ##
##########################
numbers=$(cat $FILE | grep "DiffC:" | grep -oE "[0-9]+")
echo "DISPATCH"
process_numbers "$numbers"

##############################
## Serializer dispatch time ##
##############################
numbers=$(cat $FILE_INTERNAL | grep "Diff:" | grep -oE "[0-9]+")
echo -e "\nSERIALIZER"
process_numbers "$numbers"

#################################
## Destination processing time ##
#################################
numbers=$(cat $FILE | grep "Time to apply remote on destination:" | grep -oE "[0-9]+")
echo -e "\nDESTINATION"
process_numbers "$numbers"

###################################
## Individual leaf dispatch time ##
###################################
id=1
for f in $FILES_LEAVES
do
    numbers=$(cat $f | grep "DiffC:" | grep -oE "[0-9]+")
    echo -e "\nDISPATCH Leaf$id"
    process_numbers "$numbers"
    let id=id+1
done