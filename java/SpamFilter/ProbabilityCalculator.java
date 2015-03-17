package spam;

import java.io.BufferedReader;
import java.io.FileReader;
import java.util.HashMap;
import java.util.Map;

public class ProbabilityCalculator {
	private static final String WORDBANK_FILE = "C:/Users/Ben/workspace/JavaProjects/src/spam/WordMap.csv";
	
	//All words in map are lowercase.
	//Probability map contains: <word, { P(word is in spam message), P(word is in real message) }>
	private Map<String, double[]> probabilityMap;
	private SpamAlgorithm algorithm;
	
	public ProbabilityCalculator(SpamAlgorithm algorithm) {
		if(algorithm == null)
			throw new IllegalArgumentException("Algorithm cannot be null");
		
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
