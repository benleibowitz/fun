package spam;
import java.util.Map;

public class NaiveBayesAlgorithm implements SpamAlgorithm {
	
	@Override
	public boolean isSpam(Message message, Map<String, double[]> probabilityMap) {
		double probabilitySpam = 0;
		double probabilityReal = 0;
		double sumLogsSpam = 0;
		double sumLogsReal = 0;
		
		String[] bodyWords = message.getBodyWords();
		
		/*
		 * For each word in message body, check the word and
		 * combo of (previous word + current word) for matches in the mapping.
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
						&& (Math.abs(0.5 - probabilityMap.get(wordOrPhrase)[0]) > LEGITIMATE_WORD_THRESHOLD)) {
					
					//Calculate probability of spam / real
					double probSpamWord = probabilityMap.get(wordOrPhrase)[0];
					double probRealWord = probabilityMap.get(wordOrPhrase)[1];

					//Don't want 0 numerator, as Math.log(0) returns negative infinity.
					if(probSpamWord == 0)
						probSpamWord = 0.05;
					if(probRealWord == 0)
						probRealWord = 0.05;
					
					double pSpamNumerator = probSpamWord * PROBABILITY_SPAM_MESSAGE;
					double pRealNumerator = probRealWord * (1 - PROBABILITY_SPAM_MESSAGE);
					double pDenom = (probSpamWord * PROBABILITY_SPAM_MESSAGE)
							+ (probRealWord * (1 - PROBABILITY_SPAM_MESSAGE));
					
					sumLogsSpam +=
							(Math.log(1 - pSpamNumerator/pDenom) - Math.log(pSpamNumerator/pDenom));
					sumLogsReal +=
							(Math.log(1 - pRealNumerator/pDenom) - Math.log(pRealNumerator/pDenom));
					
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
