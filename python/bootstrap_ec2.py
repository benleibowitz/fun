#!/usr/bin/python
'''Command line arguments:
	--key-file = AWS .pem key file for Chef

Gets list of all EC2 nodes on AWS account, and calls boostrap_nodes.sh
script to have Chef boostrap them.

'''
import argparse
import json
import subprocess

#Add command line parser arguments for --key-file
parser = argparse.ArgumentParser()
parser.add_argument("--key-file", type=str, required=True,
                    help="key file for chef to use for AWS")
args = parser.parse_args()

#call AWS command line client describing instances
output = subprocess.Popen('aws ec2 describe-instances', shell=True, stdout=subprocess.PIPE)
response = output.communicate()[0]

json_response = json.loads(response.decode('utf-8'))

#nodes array of public DNS names
nodes = []

for reservation in json_response['Reservations']:
    for instance in reservation['Instances']:
        public_dns = instance['PublicDnsName']
        nodes.append(public_dns)

#call bash script which bootstraps each of the nodes, adds the analytics role, and runs chef-client on each node
output = subprocess.Popen(['./bootstrap_nodes.sh %s %s ' % (args.key_file, ' '.join(nodes))], shell=True)
