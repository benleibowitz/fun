package com.ben.mapreduce;

import java.io.IOException;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.DoubleWritable;
import org.apache.hadoop.io.IntWritable;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.Job;
import org.apache.hadoop.mapreduce.Mapper;
import org.apache.hadoop.mapreduce.Reducer;
import org.apache.hadoop.mapreduce.lib.input.FileInputFormat;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.datastax.driver.core.BoundStatement;
import com.datastax.driver.core.Cluster;
import com.datastax.driver.core.PreparedStatement;
import com.datastax.driver.core.Session;

/**
 * Reads a CSV file from HDFS, maps and reduces it, and places the results in a
 * Cassandra database.
 * 
 * The CSV file in this example is about EKG data from the UCI machine learning
 * dataset repository: https://archive.ics.uci.edu/ml/datasets/Echocardiogram
 * 
 * Typically run as: hadoop -jar ThisJar.jar
 * 
 * Command line args: arg[0] = inputPath arg[1] = outputPath
 */
public class EffusionAgeMapReduceJob {

	/**
	 * Cassandra Cluster
	 */
	private static Cluster cluster;

	/**
	 * Cassandra Session
	 */
	private static Session session;

	/**
	 * Cassandra keyspace name
	 */
	private final static String KEYSPACE = "benspace";

	/**
	 * Cassandra column family for results
	 */
	private final static String OUTPUT_COLUMN_FAMILY = "effusion_age";

	/**
	 * Logger
	 */
	private static final Logger LOG = LoggerFactory
			.getLogger(EffusionAgeMapReduceJob.class);

	/**
	 * Map each effusion value (0 or 1) to the value of that patient's age
	 */
	public static class TokenizerMapper extends
			Mapper<Object, Text, IntWritable, DoubleWritable> {
		/* Input KEY, Input VALUE, Output KEY, Output VALUE */

		public void map(Object key, Text value, Context context)
				throws IOException, InterruptedException {

			String[] line = value.toString().split(",");
			try {
				double age = (double) Math.round(Double.valueOf(line[2]));
				int effusion = Integer.valueOf(line[3]);
				context.write(new IntWritable(effusion),
						new DoubleWritable(age));
			} catch (NumberFormatException e) {
				LOG.warn(String.format("Caught number format exception. "
						+ "Maybe bad age value: %s", line[2]));
			}
		}
	}

	/**
	 * Reduce the results by the average age of the effusion patients, and the
	 * average age of the non-effusion patients.
	 * 
	 * Output results to Cassandra
	 */
	public static class IntSumReducer extends
			Reducer<IntWritable, DoubleWritable, IntWritable, DoubleWritable> {

		/**
		 * Connect to Cassandra cluster
		 */
		public void setup(Context context) {
			cluster = Cluster.builder().addContactPoint("127.0.0.1").build();
			session = cluster.connect(KEYSPACE);
		}

		/**
		 * Close Cassandra session
		 */
		public void cleanup(Context context) {
			LOG.info("Closing Cassandra session");
			session.close();
		}

		public void reduce(IntWritable key, Iterable<DoubleWritable> values,
				Context context) throws IOException, InterruptedException {

			int count = 0;
			double sum = 0;
			for (DoubleWritable val : values) {
				sum += val.get();
				count += 1;
			}

			double avg = (double) sum / count;
			int effusion = Integer.valueOf(key.get());

			context.write(key, new DoubleWritable(avg));

			PreparedStatement stmt = session.prepare("INSERT INTO "
					+ OUTPUT_COLUMN_FAMILY
					+ " (effusion, avg_age) VALUES (?, ?);");
			BoundStatement bStmt = new BoundStatement(stmt);
			session.execute(bStmt.bind((effusion == 1 ? true : false), avg));
		}
	}

	public static void main(String[] args) throws Exception {

		if (args.length != 2) {
			LOG.error("Usage: [inputPath] [outputPath]");
			System.exit(1);
		}

		Configuration conf = new Configuration();
		conf.setBoolean("mapreduce.job.hduser1.classpath.first", true);
		Job job = Job.getInstance(conf, "lang count");

		job.setJarByClass(EffusionAgeMapReduceJob.class);
		job.setMapperClass(TokenizerMapper.class);
		job.setCombinerClass(IntSumReducer.class);
		job.setReducerClass(IntSumReducer.class);

		job.setOutputKeyClass(IntWritable.class);
		job.setOutputValueClass(DoubleWritable.class);

		// job.setOutputFormatClass(CqlOutputFormat.class);
		FileInputFormat.addInputPath(job, new Path(args[0]));
		FileOutputFormat.setOutputPath(job, new Path(args[1]));

		/*
		 * ConfigHelper.setOutputColumnFamily(job.getConfiguration(), KEYSPACE,
		 * OUTPUT_COLUMN_FAMILY);
		 * ConfigHelper.setOutputRpcPort(job.getConfiguration(), "9160");
		 * ConfigHelper.setOutputInitialAddress(job.getConfiguration(),
		 * "localhost");
		 * ConfigHelper.setOutputPartitioner(job.getConfiguration(),
		 * "org.apache.cassandra.dht.RandomPartitioner"); List<ByteBuffer>
		 * colNames = new ArrayList<ByteBuffer>() {{
		 * add(ByteBuffer.wrap("effusion".getBytes(Charset.forName("UTF-8"))));
		 * add(ByteBuffer.wrap("avg_age".getBytes(Charset.forName("UTF-8"))));
		 * }}; SlicePredicate predicate = new
		 * SlicePredicate().setColumn_names(colNames);
		 */
		System.exit(job.waitForCompletion(true) ? 0 : 1);
	}
}