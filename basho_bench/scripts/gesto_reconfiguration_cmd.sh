#!/bin/bash

########################
## INTERNAL FUNCTIONS ##
########################

# Gets the cloudlet IP
function get_ip_from_id {
    cloudlet_id=$1

    # Search for the right IP address
    counter=0
    for leaf in $LEAVES
    do
        if [ $counter -eq $cloudlet_id ]
        then
            cloudlet_ip=$leaf
        fi
        let counter=counter+1
    done

    # Return the IP address
    echo $cloudlet_ip
}

# Runs the Erlang command: args must come as a single string
function run_erl {
    method=$1
    args=$2
    
    # Run the reconfiguration command
    current_dir=$(pwd)
    cd scripts/erlang
    erl -pa script -name "reconfig@$IP" -setcookie saturn_leaf -run reconfig $method "$args" -run init stop
    cd $current_dir
}

####################
## MENU FUNCTIONS ##
####################

# Shows the available options at the end of each loop
function show_options {
    
    # Leave an interval between the two menu iterations
    echo ""
    
    i=1
    for op in "${options[@]}"; do
        echo "$i) $op"
        i=$((i+1))
    done
}

# Called to make a cloudlet leave
function cloudlet_leaves {
    read -p "Which cloudlet is leaving? (cloudlets go from $N_MIN to $N_CLOUDLETS) " cloudlet_id

    # Check if the user inserted some random value (only works with integers)
    if [ $cloudlet_id -lt $N_MIN ] || [ $cloudlet_id -gt $N_CLOUDLETS ]
    then
        echo "That will not work. Please try again."
        return
    fi

    cloudlet_ip=$(get_ip_from_id $cloudlet_id)
    let cloudlet_nr=cloudlet_id+1
    run_erl leave "leafs$cloudlet_nr@$cloudlet_ip $cloudlet_id"
}

# Called to make a cloudlet join
function cloudlet_joins {
    read -p "Which cloudlet is joining? (cloudlets go from $N_MIN to $N_CLOUDLETS) " cloudlet_id

    # Check if the user inserted some random value (only works with integers)
    if [ $cloudlet_id -lt $N_MIN ] || [ $cloudlet_id -gt $N_CLOUDLETS ]
    then
        echo "That will not work. Please try again."
        return
    fi

    cloudlet_ip=$(get_ip_from_id $cloudlet_id)
    let cloudlet_nr=cloudlet_id+1
    run_erl join "leafs$cloudlet_nr@$cloudlet_ip $cloudlet_id"
}

# Called to make a cloudlet fail (shutdown Erlang VM)
function cloudlet_fails {
    read -p "Which cloudlet is failing? (cloudlets go from $N_MIN to $N_CLOUDLETS) " cloudlet_id

    # Check if the user inserted some random value (only works with integers)
    if [ $cloudlet_id -lt $N_MIN ] || [ $cloudlet_id -gt $N_CLOUDLETS ]
    then
        echo "That will not work. Please try again."
        return
    fi

    cloudlet_ip=$(get_ip_from_id $cloudlet_id)
    let cloudlet_nr=cloudlet_id+1
    run_erl fail "leafs$cloudlet_nr@$cloudlet_ip"
}

# Called to make a cloudlet replicate new buckets
function replicate_group {
    read -p "Which cloudlet is replicating? (cloudlets go from $N_MIN to $N_CLOUDLETS) " cloudlet_id
    read -p "Which buckets will be replicated? (buckets are integers) " buckets

    # Check if the user inserted some random value (only works with integers)
    if [ $cloudlet_id -lt $N_MIN ] || [ $cloudlet_id -gt $N_CLOUDLETS ]
    then
        echo "That will not work. Please try again."
        return
    fi

    cloudlet_ip=$(get_ip_from_id $cloudlet_id)
    let cloudlet_nr=cloudlet_id+1
    run_erl subscribe_buckets "leafs$cloudlet_nr@$cloudlet_ip $cloudlet_id $buckets"
}

# Called to make a cloudlet replicate new objects
function replicate_object {
    read -p "Which cloudlet is replicating? (cloudlets go from $N_MIN to $N_CLOUDLETS) " cloudlet_id
    read -p "Which objects will be replicated? (bucket,object; -1 means everyone) " objects

    # Check if the user inserted some random value (only works with integers)
    if [ $cloudlet_id -lt $N_MIN ] || [ $cloudlet_id -gt $N_CLOUDLETS ]
    then
        echo "That will not work. Please try again."
        return
    fi

    cloudlet_ip=$(get_ip_from_id $cloudlet_id)
    let cloudlet_nr=cloudlet_id+1
    run_erl subscribe_objects "leafs$cloudlet_nr@$cloudlet_ip $cloudlet_id $objects"
}

###############
## CONSTANTS ##
###############
# For the menu
PS3="What do you wish to do? "
options=("Cloudlet joins" "Cloudlet leaves" "Cloudlet fails" "Replicate new object" "Replicate new group" "Exit")

# For the cloudlet connection
IP=$(dig +short myip.opendns.com @resolver1.opendns.com)
LEAVES=$(cat scripts/leafs)
N_CLOUDLETS=$(echo "$LEAVES" | wc -l)
let N_CLOUDLETS=N_CLOUDLETS-1 # Take the DC
N_MIN=1 # The first cloudlet ID

##########
## MENU ##
##########

# Welcome message
echo "Welcome to Gesto's reconfiguration menu!"
echo "You can choose each option by providing its corresponding number."
echo ""

# Interactive menu  
select opt in "${options[@]}"
do
    case $opt in
        "Cloudlet joins")
            cloudlet_joins
            ;;
        "Cloudlet leaves")
            cloudlet_leaves
            ;;
        "Cloudlet fails")
            cloudlet_fails
            ;;
        "Replicate new object")
            replicate_object
            ;;
        "Replicate new group")
            replicate_group
            ;;
        "Exit")
            echo "Goodbye."
            break
            ;;
    esac

    show_options
done