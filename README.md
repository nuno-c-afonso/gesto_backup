# Install Erlang (install_erlang.sh)

* The script was created for a machine running Ubuntu 14.04
* It installs Erlang as well as all the necessary dependencies

<!------------------------------------------------------------------>

# Reservation of machines (Grid'5000)

* Before using the provided scripts, be sure that the SSH configuration matches the tips provided on https://www.grid5000.fr/mediawiki/index.php/SSH (specially, **proxycommand** and **ssh-agent**)
* Change the **USERNAME** on the scripts to the one associated with the account that will connect to Grid'5000

## Requesting a reservation (basho_bench/scripts/open_grid5000.sh)

* Consider the [usage policy](https://www.grid5000.fr/mediawiki/index.php/Grid5000:UsagePolicy) while planning a reservation, for preventing any violation to the terms
* Set the global variables to match the request

**NOTE**: If the connection between your machine and the manager drops, the reservation is automatically cancelled after transferring the obtained results.

## Cancelling the reservation (basho_bench/scripts/stop_grid5000.sh)

* Just run the script, but confirm that there were no SSH connection rejections from the server
* If there were any, rerun the script

<!------------------------------------------------------------------>

# Server code

* **saturn_client_receiver.erl**: receives the request from the client proxy
* **saturn_leaf.erl**: acts as another indirection level for deciding where to process the client request
* **saturn_proxy_vnode.erl**: is the partition that stores the updates and replies to the majority of client requests
* **saturn_leaf_producer.erl**: receives update metadata from all local partitions, applies site stabilization and forwards the stable metadata to the broker
* **saturn_data_receiver.erl**: receives the update data, if there are differences in the propagation of the payload and metadata
* **saturn_leaf_converger.erl**: receives the update metadata on the destination server
* **saturn_internal_serv.erl**: is the broker (a.k.a serializer) that controls the propagation of update metadata
* **stats_cdf_handler.erl**: controls the recovery of some results from the servers

<!------------------------------------------------------------------>

# Client code (drivers on basho_bench/src)

* Each **run** is a different operation
* The server calls follow a similar structure to the ones in any **gesto_benchmarks** file

<!------------------------------------------------------------------>

# Bucket files

* A **bucket file** specifies the partitions that each node replicates
* Each line follows:

<partition_id>,<sequence_node_ids>

<!------------------------------------------------------------------>

# Tree files

* A **tree file** specifies how nodes are connected for the propagation of update metadata
* For only server nodes, the first line is the number of replicas
* Negative numbers mean that there is no connection among the nodes
* Otherwise, it is the delay applied to the propagation of metadata

<!------------------------------------------------------------------>

# Run the experiments

## Configuration of the client driver

* The parameters are set in **basho_bench/scripts/benchmark_manager.sh**
* **basho_bench/scripts/run_bench_grid5000.sh** runs the driver with the previous parameters

## run_all_experiments.sh

### Global variables

* **NODES**: IP addresses of the internal nodes followed by the corresponding server nodes
* **MANAGER**: IP address of the node inside Grid'5000 that controls the experiments
* **BENCH**: IP addresses of the nodes that will act as clients (the order must match the one from the server nodes)
* **SATURN_TREEFILE**: filename of the tree used in Saturn
* **CLOSEST_REPLICA**: filename of the closest pairs of Gesto instances
* **KEYNAME**: filename of the SSH key for authenticating the communication among the nodes

### Choose the experiments

* At the end of the script, add the call to the desired script that contains the experiment (there are some examples at the **experiments** directory)
