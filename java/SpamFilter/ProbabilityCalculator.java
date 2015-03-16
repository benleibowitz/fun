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
	private static ProbabilityCalculator _instance;
	
	private ProbabilityCalculator() {
	}
	
	private void initialize() {
		probabilityMap = new HashMap<>();
		
		//TEST read CSV word file
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
	
	public static ProbabilityCalculator getInstance() {
		if(_instance == null) {
			_instance = new ProbabilityCalculator();
			_instance.initialize();
		}
		
		return _instance;
	}
	
	//TODO - make algorithm class interchangeable, maybe like so:
	//public boolean isSpam(Message message, SpamAlgorithm myAlgo) {
	public boolean isSpam(Message message) {
		double probabilitySpam = 0;
		double probabilityReal = 0;
		double sumLogsSpam = 0;
		double sumLogsReal = 0;
		
		String[] bodyWords = message.getBodyWords();
		
		/*
		 * For each word in message body, check the word and
		 * combo of (previous word + current word) for matches in the mapping.
		 * If found, 
		 */
		for(int i = 0; i < bodyWords.length; i++) {
			String word = bodyWords[i];
			String adjacentWords;
			String[] wordCombos = new String[]{word};
			
			if(i > 0) {
				adjacentWords = bodyWords[i] + " " + word;
				wordCombos = new String[]{word, adjacentWords};
			}
				
			for(String wordOrPhrase : wordCombos) {
				
				//Check threshold and compute individual word
				if(probabilityMap.containsKey(wordOrPhrase)
						&& Math.abs(0.5 - probabilityMap.get(wordOrPhrase)[0]) > LEGITIMATE_WORD_THRESHOLD) {
					
					double probSpamWord = probabilityMap.get(wordOrPhrase)[0];
					double probRealWord = probabilityMap.get(wordOrPhrase)[1];
					
					//Don't want 0 numerator, as Math.log(0) returns negative infinity.
					if(probSpamWord == 0)
						probSpamWord = 0.001;
					if(probRealWord == 0)
						probSpamWord = 0.001;
					
					double pSpamNumerator = probSpamWord * PROBABILITY_SPAM_MESSAGE;
					double pRealNumerator = probRealWord * (1 - PROBABILITY_SPAM_MESSAGE);
					double pDenom = (probSpamWord * PROBABILITY_SPAM_MESSAGE)
							+ (probRealWord * (1 - PROBABILITY_SPAM_MESSAGE));
					
					sumLogsSpam += (Math.log(1 - pSpamNumerator/pDenom) - Math.log(pSpamNumerator/pDenom));
					sumLogsReal += (Math.log(1 - pRealNumerator/pDenom) - Math.log(pRealNumerator/pDenom));
				}
			}
		}
		
		probabilitySpam = 1 / (1 + Math.pow(Math.E, sumLogsSpam));
		probabilityReal = 1 / (1 + Math.pow(Math.E, sumLogsReal));
		
		//Testing
		System.out.println("Spam:" + probabilitySpam + " Real:" + probabilityReal);
		
		if(probabilitySpam > probabilityReal)
			return true;
		else
			return false;
	}
}
