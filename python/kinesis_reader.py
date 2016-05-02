#!/usr/bin/python
'''This script reads records from an AWS Kinesis
   stream, and depends on your AWS
   credentials file. (~/.aws/credentials usually)

   It needs these command line arguments:
   -s streamname
   -r region
'''

import argparse
from boto import kinesis
import json
import time

def read(region, stream_name):
    conn = kinesis.connect_to_region(region)

    stream_data = conn.describe_stream(stream_name)

    shards = stream_data['StreamDescription']['Shards']
    for shard in shards:
        shard_id = shard['ShardId']
        shard_iterator = conn.get_shard_iterator(stream_name, shard_id, 'LATEST')['ShardIterator']
        while True:
            out = conn.get_records(shard_iterator, limit=2)
            shard_iterator = out['NextShardIterator']

            for record in out['Records']:
                data = json.loads(record['Data'])
                print 'Record: ', record['Data'][:50], '....'

            time.sleep(0.2)

def parse_cmd_args():
    parser = argparse.ArgumentParser()
    parser.add_argument('-s', '--stream', dest='stream_name', required=True,
            help='The stream to read from', metavar='STREAM_NAME')
    parser.add_argument('-r', '--region', dest='region', required=True,
            help='The region to look for the Kinesis Stream in', metavar="REGION")
    return parser.parse_args()

if __name__ == '__main__':
    args = parse_cmd_args()
    read(args.region, args.stream_name)
