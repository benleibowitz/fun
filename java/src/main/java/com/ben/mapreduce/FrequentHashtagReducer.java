package com.ben.mapreduce;

import java.io.IOException;

import org.apache.hadoop.io.IntWritable;
import org.apache.hadoop.io.NullWritable;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.Reducer;
import org.bson.BasicBSONObject;

import com.google.common.collect.Lists;
import com.mongodb.hadoop.io.MongoUpdateWritable;

public class FrequentHashtagReducer extends
		Reducer<Text, IntWritable, NullWritable, MongoUpdateWritable> {

	/**
	 * Sum the counts of all the hashtags to get their total frequencies.
	 */
	@Override
	public void reduce(final Text pKey, final Iterable<IntWritable> pValues,
			final Context pContext) throws IOException, InterruptedException {

		BasicBSONObject query = new BasicBSONObject("_id", pKey.toString());

		int sum = 0;
		for (IntWritable val : pValues) {
			sum += val.get();
		}

		BasicBSONObject count = new BasicBSONObject("count", sum);
		BasicBSONObject update = new BasicBSONObject("$set", count);

		pContext.write(null,
				new MongoUpdateWritable(query, update, true, false));
	}
}