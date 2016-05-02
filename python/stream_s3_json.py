#!/usr/bin/python
'''
  Reads a file line by line from an AWS
  S3 bucket.

  Depends on your AWS credentials file.
'''
import boto
import json
import time

def read_as_stream(bucket, f_name):
  #if we wanted to loop over files, we could use this
  #for key in bucket.list(prefix=f_name):

  key = bucket.get_key(f_name)
  if key:
    unfinished_line = ''
    for byte in key:
      byte = unfinished_line + byte
      lines = byte.split('\n')
      unfinished_line = lines.pop()
      for line in lines:
        yield line     

conn = boto.connect_s3()
bucket = conn.lookup('my-bucket')

for line in read_as_stream(bucket, 'myfile.json'):
  json_data = json.loads(line)
  print json_data['text'].encode('utf-8')
