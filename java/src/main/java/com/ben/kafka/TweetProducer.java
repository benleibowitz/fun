package com.ben.kafka;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Properties;

import kafka.javaapi.producer.Producer;
import kafka.producer.KeyedMessage;
import kafka.producer.ProducerConfig;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Pushes sample messages to a Kafka topic. Assumes there is a Kafka broker on
 * local host 9092
 */
public class TweetProducer {
	private static final Logger LOG = LoggerFactory
			.getLogger(TweetProducer.class);

	public static void main(String[] args) throws FileNotFoundException,
			IOException {
		Properties props = new Properties();
		props.put("metadata.broker.list", "localhost:9092");
		props.put("serializer.class", "kafka.serializer.StringEncoder");
		props.put("partitioner.class", "kafka.SimplePartitioner");
		props.put("request.required.acks", "1");

		ProducerConfig config = new ProducerConfig(props);

		Producer<String, String> producer = new Producer<String, String>(config);

		int i = 0;
		while (i >= 0) {
			String message = String.format("This is message number: %03d", i);
			KeyedMessage<String, String> data = new KeyedMessage<String, String>(
					"ben_topic", message);
			producer.send(data);
			if (i % 10000 == 0) {
				LOG.info(String.format("Placed: %03d messages", i));
			}
		}
		producer.close();
	}
}
