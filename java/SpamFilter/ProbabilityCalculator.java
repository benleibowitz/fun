package spam;

import java.io.BufferedReader;
import java.io.FileReader;
import java.util.HashMap;
import java.util.Map;

public class ProbabilityCalculator {
	//All words in map are lowercase.
	//Probability map contains: <word, { P(word is in spam message), P(word is in real message) }>
	private Map<String, double[]> probabilityMap;
	
	private static final String WORDBANK_FILE = "C:/Users/Ben/workspace/JavaProjects/src/spam/WordMap.csv";
	
	//Probability that an average message is spam (stats say it could be anywhere from 0.5 to 0.8)
	private static final double PROBABILITY_SPAM_MESSAGE = 0.5;
	
	/*
	 * If a word's probability of being a spam word is more than LEGITIMATE_WORD_THRESHOLD
	 * away from 0.5, (ie, how far it is from 50/50), use the word in calculation.
	 * This helps us avoid words that are 50%/50% spam/real, and Bayesian poisoning
	 * EXAMPLE: if LEGITIMATE_WORD_THRESHOLD=0.2, word will be
	 * used if probability spam word < .3 or > .7
	*/
	private static final double LEGITIMATE_WORD_THRESHOLD = 0.35;
	
	private SpamAlgorithm algorithm;
	
	public ProbabilityCalculator(SpamAlgorithm algorithm) {
		this.algorithm = algorithm;
		initialize();
	}
	
	private void initialize() {
		probabilityMap = new HashMap<>();
		
		//TEST read CSV word file
		//TODO - implement CSV reader class
		BufferedReader br = null;
		try {
			br = new BufferedReader(new FileReader(WORDBANK_FILE));
			String line;
			br.readLine();
			
			while((line = br.readLine()) != null) {
				String[] ar = line.split(",");
				double totMessages = Double.valueOf(ar[1]);
				double spamMessages = Double.valueOf(ar[2]);
				double realMessages = Double.valueOf(ar[3]);
				probabilityMap.put(ar[0], new double[]{spamMessages/totMessages, realMessages/totMessages});
			}
		} catch(Exception e) {
			e.printStackTrace();
		}
	}
	
	public boolean isSpam(Message message) {
		return algorithm.isSpam(message, probabilityMap);
	}
}
