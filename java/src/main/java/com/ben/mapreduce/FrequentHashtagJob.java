package com.ben.mapreduce;

import java.net.UnknownHostException;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.io.IntWritable;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.util.ToolRunner;

import com.mongodb.hadoop.MongoInputFormat;
import com.mongodb.hadoop.MongoOutputFormat;
import com.mongodb.hadoop.io.BSONWritable;
import com.mongodb.hadoop.util.MapredMongoConfigUtil;
import com.mongodb.hadoop.util.MongoConfigUtil;
import com.mongodb.hadoop.util.MongoTool;

public class FrequentHashtagJob extends MongoTool {

    public FrequentHashtagJob() throws UnknownHostException {
        setConf(new Configuration());

        if (MongoTool.isMapRedV1()) {
            MapredMongoConfigUtil.setInputFormat(getConf(), com.mongodb.hadoop.mapred.MongoInputFormat.class);
            MapredMongoConfigUtil.setOutputFormat(getConf(), com.mongodb.hadoop.mapred.MongoOutputFormat.class);
        } else {
            MongoConfigUtil.setInputFormat(getConf(), MongoInputFormat.class);
            MongoConfigUtil.setOutputFormat(getConf(), MongoOutputFormat.class);
        }
        
        MongoConfigUtil.setInputURI(getConf(), "mongodb://hadoop:had00p@localhost:27017/sentiment.twitter");
        MongoConfigUtil.setOutputURI(getConf(), "mongodb://hadoop:had00p@localhost:27017/sentiment.frequenthashtags");
        //MongoConfigUtil.setAuthURI(getConf(), "");
        MongoConfigUtil.setMapper(getConf(), FrequentHashtagMapper.class);
        MongoConfigUtil.setReducer(getConf(), FrequentHashtagReducer.class);
        MongoConfigUtil.setMapperOutputKey(getConf(), Text.class);
        MongoConfigUtil.setMapperOutputValue(getConf(), IntWritable.class);
        MongoConfigUtil.setOutputKey(getConf(), IntWritable.class);
        MongoConfigUtil.setOutputValue(getConf(), BSONWritable.class);
        
    }

    public static void main(final String[] pArgs) throws Exception {
        System.exit(ToolRunner.run(new FrequentHashtagJob(), pArgs));
    }
}