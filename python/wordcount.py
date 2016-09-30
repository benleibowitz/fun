'''Reads a JSON file of Tweets (like one from the Twitter Streaming API) 
from an AWS S3 bucket and gets the wordcounts across all Tweets

'''
from pyspark import SparkConf, SparkContext
from pyspark.sql import SQLContext
import json
from nltk.corpus import stopwords

ignore_words = set(stopwords.words('english'))

conf = SparkConf()
sc = SparkContext(conf = conf)

sc._jsc.hadoopConfiguration().set("fs.s3n.awsAccessKeyId", "my_aws_access_key_id")
sc._jsc.hadoopConfiguration().set("fs.s3n.awsSecretAccessKey", "my_aws_secret_access_key")

rdd = sc.textFile('s3n://my_s3_bucket/my_tweet_file.json')

json_map = rdd.map(lambda x: json.loads(x))
words = json_map.flatMap(lambda x: [w.lower() for w in x['text'].split()]).filter(lambda x: x not in ignore_words and x[0] != '#').map(lambda x: (x, 1)).reduceByKey(lambda x, y: x + y).sortBy(lambda x: -x[1])

for thing in words.takeOrdered(100, key=lambda x: -x[1]):
  print thing

words.saveAsTextFile("s3n://my_s3_bucket/wordcount_output.txt")
