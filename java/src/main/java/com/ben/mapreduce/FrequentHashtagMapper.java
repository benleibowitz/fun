package com.ben.mapreduce;

import java.io.IOException;

import org.apache.hadoop.io.IntWritable;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.Mapper;
import org.bson.BSONObject;

import com.mongodb.BasicDBList;
import com.mongodb.BasicDBObject;

public class FrequentHashtagMapper extends Mapper<Object, BSONObject, Text, IntWritable> {
	private static IntWritable one = new IntWritable(1);
	
	/**
	 * Takes MongoDB BSONObject containing a tweet, and emits each hashtag in
	 * the tweet data, with a count 1 so we can count each hashtag's frequency overall
	 */
    @Override
    public void map(Object key, BSONObject val, final Context context) 
        throws IOException, InterruptedException {
    	
    	BasicDBObject entities = (BasicDBObject)val.get("entities");
    	
    	if(entities != null) {
    		BasicDBList hashtags = (BasicDBList)entities.get("hashtags");
    		
    		for(int i = 0; i < hashtags.size(); i++) {
    			BasicDBObject hashtag = (BasicDBObject)hashtags.get(i);
    			String hashtagText = hashtag.getString("text").toLowerCase();
    			context.write(new Text(hashtagText), one);
    		}
    	}
    }
}
