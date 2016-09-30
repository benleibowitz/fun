#!/bin/bash
#
# Command line args: ./boostrap_nodes.sh [aws key file] [aws node 1 IP] [aws node 2 IP] [aws node 3 IP] ...
#
# Bootstraps each AWS EC2 node as a Chef node with analytics role.
# 


key_file=$1
shift;

for node in "$@"
do
    echo "Running knife bootstrapping on: $node"
    knife bootstrap $node -i $key_file --hint ec2
done

#add analytics role to each node, and save
knife exec -E 'nodes.transform("name:*") {|n| puts n.run_list("role[analytics]") ; n.save }'

#ssh into each node and run chef-client
knife ssh "name:*" "sudo chef-client" -i $key_file -x root
