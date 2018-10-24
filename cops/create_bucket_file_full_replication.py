import sys
import itertools

# Parse the argument and check the type
if len(sys.argv) == 1:
    print "The script needs an argument"
    exit()

try:
    n_leaves=int(sys.argv[1])
except ValueError:
    print "The argument must be an integer!"
    exit()

# Get the nodes
nodes = []
for i in range(n_leaves):
    nodes.append(str(i))

# Get the replication list
bucket = 0
all_replicas=",".join(nodes)
for i in range(1, n_leaves + 1):
    for seq in itertools.combinations(nodes, i):
        print str(bucket) + "," + all_replicas
        bucket += 1
