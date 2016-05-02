#!/usr/bin/python
'''

    Quick script to write objects to AWS Kinesis stream.

'''
from boto import kinesis
import json
import datetime
import time

STREAM_NAME = 'my-stream'
REGION = 'my-region-1'

kinesis = kinesis.connect_to_region(REGION)
stream_data = kinesis.describe_stream(STREAM_NAME)

shards = stream_data['StreamDescription']['Shards']
shard = shards[0]
shard_id = shard['ShardId']
shard_iterator = kinesis.get_shard_iterator(STREAM_NAME, shard_id, 'LATEST')['ShardIterator']

i = 0
while True:
    #create json object with current time and the count "i" of this object in loop
    p = {}
    p['time'] = str(datetime.datetime.now())
    p['num'] = i
    j = json.dumps(p)

    output = kinesis.put_record(STREAM_NAME, json.dumps(j), 'partitionkey')
    print 'Putting record:', p
    time.sleep(0.2)
    i += 1
