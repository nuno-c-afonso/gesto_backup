ssh nuafonso@<location>
oarsub -I -t deploy -l nodes=1
kadeploy3 -f $OAR_NODEFILE -a ubuntu1404_ssh-x64-min.env -k
ssh root@<machine>

# To replace particular strings in the scripts
find . -type f -exec sed -i 's/ubuntu/root/g' {} +

# To run the benchmarks (do not forget to update the IP addresses)
## Internals + Leaves
scripts/start_leafs.sh
scripts/change_tree.sh data/manager/trees/tree_threeec2.txt

scripts/change_groups.sh data/manager/buckets/three_leafs_buckets.txt
scripts/change_groups.sh data/manager/buckets/three_leafs_buckets_dc.txt

scripts/rel_leafs.sh
cd scripts/erlang/; ./init_saturntx.sh; cd ../..

# Bench
scripts/init_bench_grid5000.sh

scripts/update_groups_bench_grid5000.sh ../../files/three_leafs_buckets.txt
scripts/update_groups_bench_grid5000.sh ../../files/three_leafs_buckets_dc.txt

scripts/update_bucketfull_grid5000.sh ../../files/three_leafs_buckets.txt ../../files/saturn/tree_threeec2.saturn
scripts/update_bucketfull_grid5000.sh ../../files/three_leafs_buckets_dc.txt ../../files/saturn/tree_threeec2.saturn

scripts/run_bench_grid5000.sh
scripts/summary_grid5000.sh

# For checking the leaves' log files
scripts/loop_nuno.sh "basho_bench/scripts/get_log_files.sh"
