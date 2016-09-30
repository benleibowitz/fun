'''PySpark script for taking a JSON file of Tweets (like the ones from the Twitter Streaming API)
and finding the most frequent hashtags

Requires nltk Python module for ignoring common stopwords
'''
from pyspark import SparkConf, SparkContext
from pyspark.sql import SQLContext
import json
import nltk
from nltk.corpus import stopwords
stop = set(stopwords.words('french') + stopwords.words('spanish') + stopwords.words('english'))

conf = SparkConf()
sc = SparkContext(conf = conf)

#Can read this from a number of different sources
rdd = sc.textFile('file:////Users/ben/my_json_file.json')

json_map = rdd.map(lambda x: json.loads(x))

hashtags = (
        #make sure the json has 'entities' key
        json_map.filter(lambda x: 'entities' in x)
        #flatten the 'text' attribute of all hashtags to create one array of all hashtags
        .flatMap(lambda x: [h['text'].lower() for h in x['entities']['hashtags']])
        # make sure hashtag is not a stop word
        .filter(lambda x: x not in stop)
        # map each hashtag to the count 1 (so we can sum all the 1's and get the total count)
        .map(lambda x: (x, 1))
        # add the counts
        .reduceByKey(lambda x, y: x + y)
        # sort by highest frequency first
        .sortBy(lambda x: -x[1])
    )

# print first 200 most frequent hashtags
for thing in hashtags.takeOrdered(200, key=lambda x: -x[1]):
  print thing[0].encode('utf-8'), '\t', thing[1]

#can save the output as a text file
hashtags.saveAsTextFile('file:////Users/ben/hashtags_output.txt')
